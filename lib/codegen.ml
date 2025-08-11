(* lib/codegen.ml *)
open Ast
open Semantic

(*活变量分析优化寄存器分配策略减少lw sw 次数*)
module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)

(*******************************************************************
 * 1. 中间表示 (Intermediate Representation) 和环境定义
 *******************************************************************)

(**
 * 操作数类型。
 * 这是 codegen.mli 中 `type operand` 的具体实现。
 *)
type operand =
  | Imm of int (* 立即数, e.g., 5 *)
  | Reg of string (* 物理或虚拟寄存器, e.g., "t0", "a0" *)
  | Stack of int (* 栈上位置 (相对于fp的偏移量), e.g., -4, -8 *)
  | VReg of int (*虚拟寄存器*)

(* * 为了在没有 .mli 文件的情况下打破模块间的类型依赖，
 * 我们在内部复制一份操作符的定义。
*)
type ir_binop =
  | IR_Add
  | IR_Sub
  | IR_Mul
  | IR_Div
  | IR_Mod
  | IR_Eq
  | IR_Neq
  | IR_Lt
  | IR_Le
  | IR_Gt
  | IR_Ge
  | IR_And
  | IR_Or

type ir_unop =
  | IR_Neg
  | IR_Not

(**
 * 中间表示 (IR) 类型。
 * 这是 codegen.mli 中 `type ir` 的具体实现。
 *)
type ir =
  | Label of string
  | Li of operand * int
  | Move of operand * operand
  | BinOp of ir_binop * operand * operand * operand
  | UnOp of ir_unop * operand * operand
  | Load of operand * operand
  | Store of operand * operand (* This now ALWAYS means fp-relative store *)
  | StoreOutArg of operand * int (* NEW: Explicitly for sp-relative outgoing args *)
  | Branch of ir_binop * operand * operand * string
  | BranchZ of operand * string
  | BranchNZ of operand * string
  | Jump of string
  | PreCall of int (* MODIFIED: Was Call of string * int *)
  | Call of string
  | PostCall of int
  | Ret
  | Prologue of string * int
  | Epilogue of string * int

(* 代码生成环境 (内部使用) *)
type cg_env =
  { funcs : func_sig FuncEnv.t
  ; vars : int VarEnv.t list (* A stack of scopes，变成list存储不同作用域 *)
  ; stack_top : int ref
  ; temp_counter : int ref (* MODIFIED *)
  ; label_counter : int ref (* MODIFIED *)
  ; current_function_name : string (* NEW: Store the current function's name，用于label命名 *)
  }

(* 在作用域栈中查找变量 *)
let find_var_offset (env : cg_env) (name : string) : int =
  let rec find_in_scopes scopes =
    match scopes with
    | [] -> failwith ("Undeclared variable: " ^ name)
    | current_scope :: outer_scopes ->
      (match VarEnv.find_opt name current_scope with
       | Some offset -> offset
       | None -> find_in_scopes outer_scopes)
  in
  find_in_scopes env.vars
;;

(* 在栈上为临时计算结果分配空间 *)
let alloc_temp_stack_slot env =
  env.stack_top := !(env.stack_top) - 4;
  Stack !(env.stack_top)
;;

(* 创建一个新的临时操作数。优先使用寄存器 (t0-t3)，用尽后在栈上分配空间 *)
let fresh_temp env =
  (* Renamed from fresh_temp for clarity *)
  if !(env.temp_counter) < 4
  then (
    let reg_name = "t" ^ string_of_int !(env.temp_counter) in
    env.temp_counter := !(env.temp_counter) + 1;
    Reg reg_name)
  else alloc_temp_stack_slot env
;;

let fresh_vreg env =
  let v_idx = !(env.temp_counter) in
  env.temp_counter := v_idx + 1;
  (* 不再有<4的检查，可以无限生成 "t0", "t1", "t2", ... "t100", etc. *)
  Reg ("t" ^ string_of_int v_idx)
;;

let stack_temp_counter = ref 0

(* 生成栈中转专用寄存器(t4-t6) *)
let get_stack_temp_reg () =
  (* 轮换使用 t4-t6 以避免冲突 *)
  let regs = [ "t4"; "t5"; "t6" ] in
  let idx = !stack_temp_counter mod 3 in
  stack_temp_counter := !stack_temp_counter + 1;
  Reg (List.nth regs idx)
;;

(* 创建一个新的标签 *)
let fresh_label env pfx =
  let label_name =
    Printf.sprintf "%s_%s%d" env.current_function_name pfx !(env.label_counter)
  in
  env.label_counter := !(env.label_counter) + 1;
  label_name
;;

(* 将 Ast 操作符转换为内部 IR 操作符的辅助函数 *)
let binop_from_ast_op op =
  match op with
  | Add -> IR_Add
  | Sub -> IR_Sub
  | Mul -> IR_Mul
  | Div -> IR_Div
  | Mod -> IR_Mod
  | Eq -> IR_Eq
  | Neq -> IR_Neq
  | Lt -> IR_Lt
  | Le -> IR_Le
  | Gt -> IR_Gt
  | Ge -> IR_Ge
  | And -> IR_And
  | Or -> IR_Or
;;

let unop_from_ast_op op =
  match op with
  | Neg -> IR_Neg
  | Not -> IR_Not
;;

(*活变量分析函数实现*)
(*提取operand 类型中的虚拟寄存器编号*)
let vreg_of_op op =
  match op with
  | VReg i -> Some i
  | Reg s ->
    if String.starts_with ~prefix:"t" s
    then (
      try Some (int_of_string (String.sub s 1 (String.length s - 1))) with
      | _ -> None)
    else None
  | _ -> None
;;

(* 分析单条指令的 Use 和 Def 集合 *)
(* 分析单条指令的 Use 和 Def 集合 *)
let get_use_def (instr : ir) : IntSet.t * IntSet.t =
  let use_set, def_set = ref IntSet.empty, ref IntSet.empty in
  let add_use op =
    match vreg_of_op op with
    | Some v -> use_set := IntSet.add v !use_set
    | None -> ()
  in
  let add_def op =
    match vreg_of_op op with
    | Some v -> def_set := IntSet.add v !def_set
    | None -> ()
  in
  (match instr with
   (* 定义了目标寄存器 *)
   | Li (dest, _) -> add_def dest
   | Move (dest, src) ->
     add_def dest;
     add_use src
   | Load (dest, src) ->
     add_def dest;
     add_use src
   | UnOp (_, dest, src) ->
     add_def dest;
     add_use src
   | BinOp (_, dest, src1, src2) ->
     add_def dest;
     add_use src1;
     add_use src2
   (* 只使用了源寄存器 *)
   | Store (src, dest) ->
     add_use src;
     add_use dest
   | StoreOutArg (src, _) -> add_use src
   | BranchZ (src, _) -> add_use src
   | BranchNZ (src, _) -> add_use src
   | Branch (_, src1, src2, _) ->
     add_use src1;
     add_use src2
   (*
      * 特殊情况：Call
   * 根据您的 type ir 定义，Call 只包含一个函数名 (string)。
   * 它不使用任何虚拟寄存器，因为参数的 use 已经被前面的 Load/StoreOutArg 指令捕捉。
   *)
   | Call _ -> () (* 正确的匹配：只匹配构造器，不关心其内容 *)
   (* 这些指令不使用或定义虚拟寄存器 *)
   | Label _ | Jump _ | PreCall _ | PostCall _ | Ret | Prologue _ | Epilogue _ -> ());
  !use_set, !def_set
;;

(* 对一个函数内的IR列表进行存活分析 *)
let analyze_liveness (ir_list : ir list) : IntSet.t IntMap.t =
  let live_out_map = ref IntMap.empty in
  let live_out_set = ref IntSet.empty in
  (* 当前的存活集合，从后向前流动 *)
  let num_instrs = List.length ir_list in
  (* 从后向前遍历指令 *)
  List.iteri
    (fun i instr_from_rev_list ->
       let rev_i = num_instrs - 1 - i in
       let current_instr = instr_from_rev_list in
       (* 1. 确定这条指令的后继指令的 LiveIn 集合，它们的并集就是这条指令的 LiveOut *)
       (* 在一个简单的指令列表中，后继通常是下一条指令，除非有跳转 *)
       (* 为了简化，我们暂时只考虑线性代码流，跳转的处理会让算法复杂很多 *)
       (* 对于线性流: LiveOut[i] = LiveIn[i+1] *)
       (* 对于跳转: LiveOut[i] = LiveIn[i+1] U LiveIn[target_label] *)
       (*
       一个简化的处理方式是，先忽略分支，迭代几次让信息传播。
       对于我们的线性扫描，我们只需要一个近似的存活信息即可。
       我们就用上一条指令（即后一条指令）的LiveOut作为当前指令的LiveOut的初始值。
    *)

       (* 2. 计算 Use 和 Def *)
       let use, def = get_use_def current_instr in
       (* 3. 计算 LiveIn: LiveIn = Use U (LiveOut - Def) *)
       let live_in_set = IntSet.union use (IntSet.diff !live_out_set def) in
       (* 4. 存储当前指令的 LiveOut 结果 *)
       live_out_map := IntMap.add rev_i !live_out_set !live_out_map;
       (* 5. 更新 live_out_set，为前一条指令做准备 *)
       live_out_set := live_in_set)
    (List.rev ir_list);
  !live_out_map
;;

(* 生存期间的记录类型 *)
type live_interval =
  { vreg : int (* 虚拟寄存器编号 *)
  ; start : int (* 生存开始的指令索引 *)
  ; mutable end_point : int (* 生存结束的指令索引 (可变的，方便更新) *)
  }

(*
   * 根据存活分析结果，为每个虚拟寄存器构建生存期间
 * @param ir_list 函数的IR指令列表
 * @param liveness_map 存活分析的结果 (指令索引 -> 存活集合)
 * @return (虚拟寄存器编号 -> 生存期间) 的映射
*)
let build_live_intervals (ir_list : ir list) (liveness_map : IntSet.t IntMap.t)
  : live_interval IntMap.t
  =
  let intervals = ref IntMap.empty in
  (* 遍历所有指令，找出每个vreg的定义点和使用点 *)
  List.iteri
    (fun i instr ->
       let _, def = get_use_def instr in
       (* 处理定义点：如果一个vreg在这里被定义，它的生存期间至少从这里开始 *)
       IntSet.iter
         (fun v ->
            intervals := IntMap.add v { vreg = v; start = i; end_point = i } !intervals)
         def;
       (* 获取这条指令之后的存活集合 *)
       let live_out =
         IntMap.find_opt i liveness_map |> Option.value ~default:IntSet.empty
       in
       (* 更新所有存活的vreg的结束点 *)
       IntSet.iter
         (fun v ->
            match IntMap.find_opt v !intervals with
            | Some interval -> interval.end_point <- i (* 更新结束点为当前指令索引 *)
            | None -> () (* 理论上，存活的变量应该已经被定义过了 *))
         live_out)
    ir_list;
  !intervals
;;

(* 我们将要做的分配结果的类型 *)
type allocation =
  | PhysicalReg of string (* 分配到了物理寄存ator *)
  | Spilled of int (* 被溢出到栈上，值为fp的偏移量 *)

(*
   * 线性扫描寄存器分配算法
 * @param intervals 所有虚拟寄存器的生存期间
 * @param physical_regs 可供分配的物理寄存器列表
 * @return (vreg -> 分配结果) 的映射，以及溢出所需的栈空间大小
*)
let linear_scan_alloc (intervals : live_interval list) (physical_regs : string list)
  : allocation IntMap.t * int
  =
  let allocation_map = ref IntMap.empty in
  let free_regs = ref physical_regs in
  let active = ref [] in
  (* 按end_point排序的、当前活跃的interval列表 *)
  let stack_offset = ref 0 in
  (* 辅助函数：将interval插入按end_point排序的active列表 *)
  let add_to_active interval =
    active := List.sort (fun a b -> compare a.end_point b.end_point) (interval :: !active)
  in
  (* 遍历按start排序的intervals *)
  List.iter
    (fun current_interval ->
       (* 步骤1: ExpireOldIntervals - 检查并释放过期的旧区间 *)
       let expired, still_active =
         List.partition (fun act -> act.end_point < current_interval.start) !active
       in
       active := still_active;
       List.iter
         (fun exp ->
            match IntMap.find exp.vreg !allocation_map with
            | PhysicalReg reg_name -> free_regs := reg_name :: !free_regs (* 释放寄存器 *)
            | _ -> ())
         expired;
       (* 步骤2: SpillAtInterval - 如果没有空闲寄存器，则溢出 *)
       if List.length !free_regs = 0
       then (
         let spill_candidate = List.hd (List.rev !active) in
         (* active中结束点最晚的 *)
         if spill_candidate.end_point > current_interval.end_point
         then (
           (* 溢出active中的spill_candidate *)
           match IntMap.find spill_candidate.vreg !allocation_map with
           | PhysicalReg reg_name ->
             (* 将spill_candidate标记为溢出 *)
             stack_offset := !stack_offset - 4;
             allocation_map
             := IntMap.add spill_candidate.vreg (Spilled !stack_offset) !allocation_map;
             (* 把释放出的寄存器分配给current_interval *)
             allocation_map
             := IntMap.add current_interval.vreg (PhysicalReg reg_name) !allocation_map;
             (* 更新active列表 *)
             active := List.filter (fun act -> act.vreg <> spill_candidate.vreg) !active;
             add_to_active current_interval
           | _ -> failwith "Impossible: active interval not in map")
         else (
           (* 溢出current_interval自身，不改变active列表 *)
           stack_offset := !stack_offset - 4;
           allocation_map
           := IntMap.add current_interval.vreg (Spilled !stack_offset) !allocation_map))
       else (
         (* 步骤3: 分配可用寄存器 *)
         let reg_to_alloc = List.hd !free_regs in
         free_regs := List.tl !free_regs;
         allocation_map
         := IntMap.add current_interval.vreg (PhysicalReg reg_to_alloc) !allocation_map;
         add_to_active current_interval))
    intervals;
  !allocation_map, abs !stack_offset
;;

(* 一个主分配函数 *)
let allocate_registers_for_function (ir_list : ir list) =
  (* 定义可用的物理寄存器池 *)
  let available_regs = [ "t0"; "t1"; "t2"; "t3"; "t4"; "t5" ] in
  (* 1. 存活分析 *)
  let liveness = analyze_liveness ir_list in
  (* 2. 构建生存期间 *)
  let intervals_map = build_live_intervals ir_list liveness in
  let intervals_list = List.map snd (IntMap.bindings intervals_map) in
  (* 3. 按起始点排序，为线性扫描做准备 *)
  let sorted_intervals = List.sort (fun a b -> compare a.start b.start) intervals_list in
  (* 4. 执行线性扫描分配 *)
  linear_scan_alloc sorted_intervals available_regs
;;

(* 栈布局信息 *)
type stack_layout =
  { total_size : int (* 函数总的栈大小，必须16字节对齐 *)
  ; spill_size : int (* 为溢出分配的大小 *)
  }

(*
   * 辅助函数：确保一个操作数的值最终位于一个给定的物理寄存器中
 * @param op 要加载的操作数 (可能是vreg, imm, 等)
 * @param target_reg 目标物理寄存器名 (e.g., "t0")
 * @param alloc_map 分配结果
 * @param stack_layout 栈布局信息 (我们稍后会定义)
 * @return 生成的汇编指令列表
*)
let ensure_in_reg op target_reg alloc_map stack_layout =
  match op with
  | Imm i -> [ Printf.sprintf "  li %s, %d" target_reg i ]
  | Reg r ->
    (* 这已经是一个物理寄存ator了，可能是 "a0" 或 "fp" *)
    if r <> target_reg then [ Printf.sprintf "  mv %s, %s" target_reg r ] else []
  | VReg v ->
    (* 核心逻辑：处理虚拟寄存器 *)
    (match IntMap.find v alloc_map with
     | PhysicalReg reg_name ->
       (* 分配到了物理寄存器，直接移动过来 *)
       if reg_name <> target_reg
       then [ Printf.sprintf "  mv %s, %s" target_reg reg_name ]
       else []
     | Spilled offset ->
       (* 被溢出到栈上，需要从栈加载 *)
       (* 我们需要知道总的栈大小来正确计算偏移量 *)
       let final_offset = -(stack_layout.total_size - offset) in
       [ Printf.sprintf "  lw %s, %d(fp)" target_reg final_offset ])
  | Stack _ -> failwith "Stack operand should not appear in vreg IR"
;;

(*
   * 辅助函数：将一个物理寄存器中的值存回一个操作数所代表的位置
 * @param src_reg 源物理寄存器名 (e.g., "t0")
 * @param dest_op 目标操作数 (只可能是虚拟寄存器)
 * @param alloc_map 分配结果
 * @param stack_layout 栈布局信息
 * @return 生成的汇编指令列表
*)
let store_from_reg src_reg dest_op alloc_map stack_layout =
  match dest_op with
  | VReg v ->
    (match IntMap.find v alloc_map with
     | PhysicalReg reg_name ->
       (* 目标是物理寄存器，移动过去 *)
       if src_reg <> reg_name
       then [ Printf.sprintf "  mv %s, %s" reg_name src_reg ]
       else []
     | Spilled offset ->
       (* 目标在栈上，存回去 *)
       let final_offset = -(stack_layout.total_size - offset) in
       [ Printf.sprintf "  sw %s, %d(fp)" src_reg final_offset ])
  | _ -> failwith "Destination of computation must be a virtual register"
;;

(*******************************************************************
 * 2. 从 AST 到 IR 的转换 (内部函数)
 *******************************************************************)

let rec gen_expr_ir_internal env (e : expr) : ir list * operand =
  match e with
  | IntLiteral n ->
    let temp_reg = fresh_vreg env in
    [ Li (temp_reg, n) ], temp_reg
  | Id x ->
    let var_loc = find_var_offset env x in
    let temp_reg = fresh_vreg env in
    [ Load (temp_reg, Stack var_loc) ], temp_reg
  | Assign (x, rhs_expr) ->
    let rhs_ir, rhs_op = gen_expr_ir_internal env rhs_expr in
    let var_loc = find_var_offset env x in
    rhs_ir @ [ Store (rhs_op, Stack var_loc) ], rhs_op
  | UnOp (op, expr) ->
    let expr_ir, expr_op = gen_expr_ir_internal env expr in
    let dest_reg = fresh_vreg env in
    expr_ir @ [ UnOp (unop_from_ast_op op, dest_reg, expr_op) ], dest_reg
  | BinOp (op, e1, e2) ->
    (match op with
     | And ->
       let dest_op = fresh_vreg env in
       let false_label = fresh_label env "L_false_" in
       let end_label = fresh_label env "L_end_" in
       let ir1, op1 = gen_expr_ir_internal env e1 in
       env.temp_counter := 0;
       let ir2, op2 = gen_expr_ir_internal env e2 in
       ( ir1
         @ [ BranchZ (op1, false_label) ]
         @ ir2
         @ [ BranchZ (op2, false_label) ]
         @ [ Li (dest_op, 1)
           ; Jump end_label
           ; Label false_label
           ; Li (dest_op, 0)
           ; Label end_label
           ]
       , dest_op )
     | Or ->
       let dest_op = fresh_vreg env in
       let true_label = fresh_label env "L_true_" in
       let end_label = fresh_label env "L_end_" in
       let ir1, op1 = gen_expr_ir_internal env e1 in
       env.temp_counter := 0;
       let ir2, op2 = gen_expr_ir_internal env e2 in
       ( ir1
         @ [ BranchNZ (op1, true_label) ]
         @ ir2
         @ [ BranchNZ (op2, true_label) ]
         @ [ Li (dest_op, 0)
           ; Jump end_label
           ; Label true_label
           ; Li (dest_op, 1)
           ; Label end_label
           ]
       , dest_op )
       (* The robust "Spill-and-Reload" strategy, but this time with a correct assembler *)
       (* | _ ->
       (* This order of evaluation (e1 then e2) is more conventional *)
       let ir1, op1 = gen_expr_ir_internal env e1 in
       let temp_slot_for_op1 = fresh_vreg env in
       let save_ir = [ Store (op1, temp_slot_for_op1) ] in
       env.temp_counter := 0;
       (* Reset temps for the other side *)
       let ir2, op2 = gen_expr_ir_internal env e2 in
       let loaded_op1 = fresh_vreg env in
       let load_ir = [ Load (loaded_op1, temp_slot_for_op1) ] in
       let dest_op = fresh_vreg env in
       let final_op = binop_from_ast_op op in
       let full_ir = ir1 @ save_ir @ ir2 @ load_ir in
       (match final_op with
        | IR_Eq | IR_Neq | IR_Lt | IR_Le | IR_Gt | IR_Ge ->
          let true_label = fresh_label env "L_true_" in
          let end_label = fresh_label env "L_end_" in
          ( full_ir
            @ [ Li (dest_op, 0)
              ; Branch (final_op, loaded_op1, op2, true_label)
              ; Jump end_label
              ; Label true_label
              ; Li (dest_op, 1)
              ; Label end_label
              ]
          , dest_op )
        | _ -> full_ir @ [ BinOp (final_op, dest_op, loaded_op1, op2) ], dest_op)) *)
       (* 在 gen_expr_ir_internal 中，处理 BinOp 的非短路求值部分 *)
     | _ ->
       (* 处理非 "And" 和 "Or" 的所有其他二元操作 *)
       (* 1. 依次为左右操作数生成IR。op1 和 op2 现在是虚拟寄存器。*)
       let ir1, op1 = gen_expr_ir_internal env e1 in
       env.temp_counter := 0;
       let ir2, op2 = gen_expr_ir_internal env e2 in
       (* 2. 将左右两边的IR拼接起来。这就是我们新的、简化的 "full_ir" *)
       let eval_ir = ir1 @ ir2 in
       (* 3. 获取操作符的IR表示 *)
       let final_op = binop_from_ast_op op in
       (* 4. 核心逻辑：根据是算术运算还是比较运算，生成不同的IR *)
       (match final_op with
        (* 情况A：是比较运算 *)
        | IR_Eq | IR_Neq | IR_Lt | IR_Le | IR_Gt | IR_Ge ->
          let dest_vreg = fresh_vreg env in
          (* 为0或1的结果创建一个新的虚拟寄存器 *)
          let true_label = fresh_label env "L_true_" in
          let end_label = fresh_label env "L_end_" in
          (* 最终的IR = 求值IR + 分支逻辑IR。
           注意我们现在直接使用 op1，而不是 loaded_op1 *)
          ( eval_ir
            @ [ Li (dest_vreg, 0) (* 先假设结果为假(0) *)
              ; Branch (final_op, op1, op2, true_label) (* 如果 op1 < op2 (等) 为真，则跳转 *)
              ; Jump end_label (* 如果为假，则跳过设置成1的部分 *)
              ; Label true_label
              ; Li (dest_vreg, 1) (* 在这里将结果设置为真(1) *)
              ; Label end_label
              ]
          , dest_vreg )
        (* 情况B：是算术运算 *)
        | IR_Add | IR_Sub | IR_Mul | IR_Div | IR_Mod ->
          let dest_vreg = fresh_vreg env in
          (* 为算术结果创建一个新的虚拟寄存器 *)
          (* 最终的IR = 求值IR + 一条算术指令。
           同样，直接使用 op1 和 op2 *)
          eval_ir @ [ BinOp (final_op, dest_vreg, op1, op2) ], dest_vreg
        | IR_And | IR_Or ->
          failwith "FATAL: IR_And/IR_Or reached the non-short-circuiting BinOp handler."))
  | Call (fname, args) ->
    (* 步骤 1: 依次求值每个参数，并将结果保存在各自的虚拟寄存器中 *)
    let args_eval_results =
      List.map
        (fun arg_expr ->
           env.temp_counter := 0;
           (* 为每个参数的计算重置临时vreg计数器 *)
           gen_expr_ir_internal env arg_expr)
        args
    in
    (* 分离出所有参数的IR和它们最终所在的虚拟寄存器(op) *)
    let eval_ir_list = List.map fst args_eval_results in
    let arg_ops = List.map snd args_eval_results in
    (* 将所有参数求值的IR拼接在一起 *)
    let eval_ir = List.concat eval_ir_list in
    (* 步骤 2: 区分需要通过寄存器和栈传递的参数 *)
    let reg_arg_ops, stack_arg_ops =
      let rec split n lst =
        if n <= 0
        then [], lst
        else (
          match lst with
          | [] -> [], []
          | h :: t ->
            let taken, rest = split (n - 1) t in
            h :: taken, rest)
      in
      split 8 arg_ops (* 前8个用寄存器，其余用栈 *)
    in
    (* 步骤 3: 按照ABI顺序生成参数传递的IR *)
    (* 3.1: (PreCall) 为需要通过栈传递的参数预留空间 *)
    let num_stack_args = List.length stack_arg_ops in
    let stack_space_for_args = num_stack_args * 4 in
    let pre_call_ir = [ PreCall stack_space_for_args ] in
    (* 3.2: (StoreOutArg) 生成将vreg存入出参栈空间的指令 *)
    let stack_passing_ir =
      List.mapi (fun i op -> StoreOutArg (op, i * 4)) stack_arg_ops
    in
    (* 3.3: (Move) 生成将vreg移动到a0-a7物理寄存器的指令 *)
    let reg_passing_ir =
      List.mapi (fun i op -> Move (Reg ("a" ^ string_of_int i), op)) reg_arg_ops
    in
    (* 步骤 4: 生成真正的调用指令，以及后续清理和返回值处理 *)
    let return_vreg = fresh_vreg env in
    let call_cleanup_ir =
      [ Call fname
      ; PostCall stack_space_for_args
      ; Move (return_vreg, Reg "a0") (* 将返回值从a0移入一个新的vreg *)
      ]
    in
    (* 最终的 IR 顺序: 求值 -> 准备调用栈 -> 传递栈参数 -> 传递寄存器参数 -> 调用和清理 *)
    let full_ir =
      eval_ir @ pre_call_ir @ stack_passing_ir @ reg_passing_ir @ call_cleanup_ir
    in
    full_ir, return_vreg
;;

(* _ -> failwith "Unsupported expression type in codegen" *)

let rec gen_stmt_ir_internal (env : cg_env) ?break_lbl ?cont_lbl (s : stmt)
  : ir list * cg_env
  =
  env.temp_counter := 0;
  (* MODIFIED *)
  match s with
  | Expr e ->
    let ir, _ = gen_expr_ir_internal env e in
    ir, env
    (* The environment does not change for an expression statement. *)
  | Return None -> [ Ret ], env
  | Return (Some e) ->
    let ir, op = gen_expr_ir_internal env e in
    ir @ [ Move (Reg "a0", op); Ret ], env
  | VarDecl (id, init_e) ->
    (* 1. Generate IR for the initializer using the CURRENT environment. *)
    let init_ir, op = gen_expr_ir_internal env init_e in
    (* 2. Allocate stack space for the new variable. *)
    env.stack_top := !(env.stack_top) - 4;
    let var_loc = !(env.stack_top) in
    (* 3. Create the store instruction. *)
    let store_ir = [ Store (op, Stack var_loc) ] in
    (* 4. Create the NEW environment for subsequent statements by updating the vars list. *)
    let new_current_scope = VarEnv.add id var_loc (List.hd env.vars) in
    let new_env = { env with vars = new_current_scope :: List.tl env.vars } in
    init_ir @ store_ir, new_env
    (* Return the IR and the MODIFIED environment. *)
  | Block stmts ->
    (* 1. Enter a new scope by pushing an empty var map. *)
    let block_env = { env with vars = VarEnv.empty :: env.vars } in
    (* 2. Process the statements within the new scope. *)
    let block_ir, _ = gen_stmts_ir_internal block_env ?break_lbl ?cont_lbl stmts in
    (* 3. Exit the scope by returning the original environment. *)
    block_ir, env
  | If (cond, then_s, else_s_opt) ->
    (* All sub-expressions and statements will now use the same `env` instance,
         ensuring the label_counter is correctly incremented and never reset. *)
    let cond_ir, cond_op = gen_expr_ir_internal env cond in
    let else_label = fresh_label env "L_else_" in
    let end_label = fresh_label env "L_end_" in
    let then_ir, _ = gen_stmt_ir_internal env ?break_lbl ?cont_lbl then_s in
    (match else_s_opt with
     | None ->
       cond_ir @ [ BranchZ (cond_op, end_label) ] @ then_ir @ [ Label end_label ], env
     | Some else_s ->
       let else_ir, _ = gen_stmt_ir_internal env ?break_lbl ?cont_lbl else_s in
       ( cond_ir
         @ [ BranchZ (cond_op, else_label) ]
         @ then_ir
         @ [ Jump end_label; Label else_label ]
         @ else_ir
         @ [ Label end_label ]
       , env ))
  | While (cond, body) ->
    let start_label = fresh_label env "L_while_start_" in
    let end_label = fresh_label env "L_while_end_" in
    let cond_ir, cond_op = gen_expr_ir_internal env cond in
    let body_ir, _ =
      gen_stmt_ir_internal env ~break_lbl:end_label ~cont_lbl:start_label body
    in
    ( [ Label start_label ]
      @ cond_ir
      @ [ BranchZ (cond_op, end_label) ]
      @ body_ir
      @ [ Jump start_label; Label end_label ]
    , env )
  | Break ->
    (match break_lbl with
     | Some lbl -> [ Jump lbl ], env
     | None -> failwith "break statement not within a loop")
  | Continue ->
    (match cont_lbl with
     | Some lbl -> [ Jump lbl ], env
     | None -> failwith "continue statement not within a loop")

(* This helper function processes a LIST of statements,
 * passing the updated environment from one statement to the next. *)
and gen_stmts_ir_internal (env : cg_env) ?break_lbl ?cont_lbl (stmts : stmt list)
  : ir list * cg_env
  =
  List.fold_left
    (fun (acc_ir, current_env) stmt ->
       let new_ir, next_env =
         gen_stmt_ir_internal current_env ?break_lbl ?cont_lbl stmt
       in
       acc_ir @ new_ir, next_env)
    ([], env)
    stmts
;;

(* MODIFIED: This function now correctly calculates stack size after generating the body IR. *)
let gen_func_ir_internal (ana : analysis_result) (f : func_def) : ir list =
  (* 1. Setup initial environment for parameters *)
  let param_offset = ref (-8) in
  let params_with_offsets =
    List.mapi
      (fun i name ->
         param_offset := !param_offset - 4;
         let offset = if i < 8 then !param_offset else (i - 8) * 4 in
         let reg_opt = if i < 8 then Some (Reg ("a" ^ string_of_int i)) else None in
         name, offset, reg_opt)
      f.params
  in
  let initial_var_map =
    List.fold_left
      (fun acc (name, offset, _) -> VarEnv.add name offset acc)
      VarEnv.empty
      params_with_offsets
  in
  (* 2. Create the initial generation environment *)
  let env =
    { funcs = ana.global_funcs
    ; vars = [ initial_var_map ]
    ; (* Start with one scope for parameters *)
      stack_top = ref !param_offset
    ; temp_counter = ref 0
    ; (* MODIFIED *)
      label_counter = ref 0
    ; (* MODIFIED *)
      current_function_name = f.fname (* INITIALIZE HERE *)
    }
  in
  (* 3. Generate the IR for the function body using the new helper. *)
  let body_ir, final_env = gen_stmts_ir_internal env f.body in
  (* 4. Save parameters from registers to stack *)
  let params_save_ir =
    List.filter_map
      (function
        | _, offset, Some reg -> Some (Store (reg, Stack offset))
        | _ -> None)
      params_with_offsets
  in
  (* 5. Calculate final stack size using the final environment's stack_top. *)
  let required_stack = abs !(final_env.stack_top) + 8 in
  let stack_size =
    if required_stack mod 16 == 0
    then required_stack
    else required_stack + (16 - (required_stack mod 16))
  in
  (* 6. Assemble the full function IR *)
  [ Prologue (f.fname, stack_size) ]
  @ params_save_ir
  @ body_ir
  @ [ Epilogue (f.fname, stack_size) ]
;;

(* let ir_to_asm_list_internal (ir_instr : ir) : string list =
  let is_small_imm i = i >= -2048 && i <= 2047 in
  let emit_mem_access op_str reg_name offset base_reg =
    if is_small_imm offset
    then [ Printf.sprintf "  %s %s, %d(%s)" op_str reg_name offset base_reg ]
    else
      [ Printf.sprintf "  li t6, %d" offset
      ; Printf.sprintf "  add t6, %s, t6" base_reg
      ; Printf.sprintf "  %s %s, 0(t6)" op_str reg_name
      ]
  in
  let ensure_in_reg op target_reg =
    match op with
    | Reg s ->
      if s = target_reg
      then [], s
      else [ Printf.sprintf "  mv %s, %s" target_reg s ], target_reg
    | Stack i -> emit_mem_access "lw" target_reg i "fp", target_reg
    | Imm i -> [ Printf.sprintf "  li %s, %d" target_reg i ], target_reg
  in
  let store_from_reg src_reg dest_op =
    match dest_op with
    | Reg s -> if s = src_reg then [] else [ Printf.sprintf "  mv %s, %s" s src_reg ]
    | Stack i -> emit_mem_access "sw" src_reg i "fp"
    | Imm _ -> failwith "FATAL: Cannot store into an immediate value"
  in
  (*** NEW: A robust helper to load any operand into a dedicated temporary register ***)
  let load_operand_to_temp op =
    match op with
    | Reg s -> [], s (* Already a register, no load needed *)
    | Stack i ->
      let temp_reg =
        match get_stack_temp_reg () with
        | Reg s -> s
        | _ -> failwith "impossible"
      in
      emit_mem_access "lw" temp_reg i "fp", temp_reg
    | Imm i ->
      let temp_reg =
        match get_stack_temp_reg () with
        | Reg s -> s
        | _ -> failwith "impossible"
      in
      [ Printf.sprintf "  li %s, %d" temp_reg i ], temp_reg
  in
  match ir_instr with
  | Label s -> [ s ^ ":" ]
  | Li (dest, imm) ->
    let load_imm_ir = [ Printf.sprintf "  li t6, %d" imm ] in
    let store_ir = store_from_reg "t6" dest in
    load_imm_ir @ store_ir
  | Move (dest, src) ->
    let load_ir, src_reg_name = ensure_in_reg src "t6" in
    let store_ir = store_from_reg src_reg_name dest in
    load_ir @ store_ir
  | Load (dest, src) ->
    (match src with
     | Stack i ->
       let load_val_ir = emit_mem_access "lw" "t6" i "fp" in
       let store_dest_ir = store_from_reg "t6" dest in
       load_val_ir @ store_dest_ir
     | _ -> failwith "FATAL: Source of Load must be fp-relative Stack location")
  | Store (src, dest) ->
    (match dest with
     | Stack i ->
       let load_src_ir, src_reg = ensure_in_reg src "t6" in
       let store_ir = emit_mem_access "sw" src_reg i "fp" in
       load_src_ir @ store_ir
     | _ -> failwith "FATAL: Destination of Store must be fp-relative Stack location")
  | StoreOutArg (src, offset) ->
    let load_src_ir, src_reg = ensure_in_reg src "t6" in
    let store_ir = emit_mem_access "sw" src_reg offset "sp" in
    load_src_ir @ store_ir
  | UnOp (op, dest, src) ->
    let op_str =
      match op with
      | IR_Neg -> "neg"
      | IR_Not -> "seqz"
    in
    let load_ir, src_reg = ensure_in_reg src "t6" in
    let compute_ir = [ Printf.sprintf "  %s t6, %s" op_str src_reg ] in
    let store_ir = store_from_reg "t6" dest in
    load_ir @ compute_ir @ store_ir
  (*** MODIFIED, ROBUST BinOp ASSEMBLY LOGIC ***)
  | BinOp (op, dest, src1, src2) ->
    stack_temp_counter := 0;
    (* Reset temp register pool for each instruction *)
    let op_str =
      match op with
      | IR_Add -> "add"
      | IR_Sub -> "sub"
      | IR_Mul -> "mul"
      | IR_Div -> "div"
      | IR_Mod -> "rem"
      | _ -> failwith "Invalid op for BinOp"
    in
    (* 1. Safely load both source operands into dedicated temp registers (t4, t5) *)
    let load1_ir, r1 = load_operand_to_temp src1 in
    let load2_ir, r2 = load_operand_to_temp src2 in
    (* 2. Get a third temp register (t6) to store the result of the computation *)
    let result_reg =
      match get_stack_temp_reg () with
      | Reg s -> s
      | _ -> failwith "impossible"
    in
    let compute_ir = [ Printf.sprintf "  %s %s, %s, %s" op_str result_reg r1 r2 ] in
    (* 3. Store the result from the temp register to the final destination *)
    let store_ir = store_from_reg result_reg dest in
    load1_ir @ load2_ir @ compute_ir @ store_ir
  | BranchZ (src, label) ->
    let load_ir, reg = ensure_in_reg src "t6" in
    load_ir @ [ Printf.sprintf "  beqz %s, %s" reg label ]
  | BranchNZ (src, label) ->
    let load_ir, reg = ensure_in_reg src "t6" in
    load_ir @ [ Printf.sprintf "  bnez %s, %s" reg label ]
  (*** MODIFIED, ROBUST Branch ASSEMBLY LOGIC ***)
  | Branch (op, src1, src2, label) ->
    stack_temp_counter := 0;
    (* Reset temp register pool *)
    let branch_op_str =
      match op with
      | IR_Eq -> "beq"
      | IR_Neq -> "bne"
      | IR_Lt -> "blt"
      | IR_Le -> "ble"
      | IR_Gt -> "bgt"
      | IR_Ge -> "bge"
      | _ -> failwith "Invalid op for Branch"
    in
    (* 1. Safely load both source operands into dedicated temp registers *)
    let load1_ir, r1 = load_operand_to_temp src1 in
    let load2_ir, r2 = load_operand_to_temp src2 in
    (* 2. Perform the branch comparison *)
    let branch_ir = [ Printf.sprintf "  %s %s, %s, %s" branch_op_str r1 r2 label ] in
    load1_ir @ load2_ir @ branch_ir
  (* ... All other cases from Jump to Epilogue remain the same ... *)
  | Jump s -> [ Printf.sprintf "  j %s" s ]
  | Ret -> failwith "Ret should not be directly converted, it's handled by Epilogue"
  | PreCall stack_space ->
    if stack_space > 0
    then
      if is_small_imm (-stack_space)
      then [ Printf.sprintf "  addi sp, sp, -%d" stack_space ]
      else [ Printf.sprintf "  li t6, %d" stack_space; Printf.sprintf "  sub sp, sp, t6" ]
    else []
  | Call s -> [ Printf.sprintf "  call %s" s ]
  | PostCall stack_space ->
    if stack_space > 0
    then
      if is_small_imm stack_space
      then [ Printf.sprintf "  addi sp, sp, %d" stack_space ]
      else [ Printf.sprintf "  li t6, %d" stack_space; Printf.sprintf "  add sp, sp, t6" ]
    else []
  | Prologue (fname, stack_size) ->
    let setup_sp =
      if is_small_imm (-stack_size)
      then [ Printf.sprintf "  addi sp, sp, -%d" stack_size ]
      else [ Printf.sprintf "  li t6, %d" stack_size; Printf.sprintf "  sub sp, sp, t6" ]
    in
    let save_ra = emit_mem_access "sw" "ra" (stack_size - 4) "sp" in
    let save_fp = emit_mem_access "sw" "fp" (stack_size - 8) "sp" in
    let setup_fp =
      if is_small_imm stack_size
      then [ Printf.sprintf "  addi fp, sp, %d" stack_size ]
      else [ Printf.sprintf "   li t6, %d" stack_size; Printf.sprintf "  add fp, sp, t6" ]
    in
    [ ".text"; ".globl " ^ fname; fname ^ ":" ] @ setup_sp @ save_ra @ save_fp @ setup_fp
  | Epilogue (fname, stack_size) ->
    let restore_fp = emit_mem_access "lw" "fp" (stack_size - 8) "sp" in
    let restore_ra = emit_mem_access "lw" "ra" (stack_size - 4) "sp" in
    let teardown_sp =
      if is_small_imm stack_size
      then [ Printf.sprintf "  addi sp, sp, %d" stack_size ]
      else [ Printf.sprintf "  li t6, %d" stack_size; Printf.sprintf "  add sp, sp, t6" ]
    in
    [ ".L_ret_" ^ fname ^ ":" ] @ restore_fp @ restore_ra @ teardown_sp @ [ "  ret" ]
;; *)
(*
 * 最终的汇编生成函数：将一条IR指令根据分配结果转换为汇编代码
 * @param ir_instr 要转换的单条IR指令
 * @param alloc_map 寄存器分配结果
 * @param stack_layout 当前函数的栈布局信息
 * @return 代表该指令的一或多条汇编指令字符串列表
 *)
let ir_to_asm_list_final
      (ir_instr : ir)
      (alloc_map : allocation IntMap.t)
      (stack_layout : stack_layout)
  : string list
  =
  (* 定义用于本指令内部计算的临时物理寄存器。
     注意：这些寄存器绝不能出现在 alloc_map 的 PhysicalReg 池中！
     我们假设 t0-t4 被分配器使用，t5, t6 留作指令生成时的 "暂存器"。*)
  let temp_reg1 = "t5" in
  let temp_reg2 = "t6" in
  match ir_instr with
  (* 1. 为目标(vreg)赋值的指令 *)
  | Li (dest, imm) ->
    (* 步骤: li t5, imm -> store t5 to dest *)
    let load_imm = [ Printf.sprintf "  li %s, %d" temp_reg1 imm ] in
    let store_result = store_from_reg temp_reg1 dest alloc_map stack_layout in
    load_imm @ store_result
  | Move (dest, src) ->
    (* 步骤: load src to t5 -> store t5 to dest *)
    let load_src = ensure_in_reg src temp_reg1 alloc_map stack_layout in
    let store_result = store_from_reg temp_reg1 dest alloc_map stack_layout in
    load_src @ store_result
  | Load (dest, src) ->
    (* Load 指令的源必须是地址，这里我们假设它是一个虚拟寄存器，
         其内容是地址。这部分可能需要根据你的IR设计微调。
         为了演示，我们假设 src 是一个包含地址的 vreg。*)
    let load_addr = ensure_in_reg src temp_reg1 alloc_map stack_layout in
    let do_load = [ Printf.sprintf "  lw %s, 0(%s)" temp_reg1 temp_reg1 ] in
    (* t5 = *(t5) *)
    let store_result = store_from_reg temp_reg1 dest alloc_map stack_layout in
    load_addr @ do_load @ store_result
  | UnOp (op, dest, src) ->
    (* 步骤: load src to t5 -> op t5, t5 -> store t5 to dest *)
    let load_src = ensure_in_reg src temp_reg1 alloc_map stack_layout in
    let op_str =
      match op with
      | IR_Neg -> "neg"
      | IR_Not -> "seqz"
    in
    let compute = [ Printf.sprintf "  %s %s, %s" op_str temp_reg1 temp_reg1 ] in
    let store_result = store_from_reg temp_reg1 dest alloc_map stack_layout in
    load_src @ compute @ store_result
  | BinOp (op, dest, src1, src2) ->
    (* 步骤: load src1 to t5 -> load src2 to t6 -> op t5, t5, t6 -> store t5 to dest *)
    let load_src1 = ensure_in_reg src1 temp_reg1 alloc_map stack_layout in
    let load_src2 = ensure_in_reg src2 temp_reg2 alloc_map stack_layout in
    let op_str =
      match op with
      | IR_Add -> "add"
      | IR_Sub -> "sub"
      | IR_Mul -> "mul"
      | IR_Div -> "div"
      | IR_Mod -> "rem"
      | _ -> failwith "BinOp instruction has non-arithmetic operator"
    in
    let compute =
      [ Printf.sprintf "  %s %s, %s, %s" op_str temp_reg1 temp_reg1 temp_reg2 ]
    in
    let store_result = store_from_reg temp_reg1 dest alloc_map stack_layout in
    load_src1 @ load_src2 @ compute @ store_result
  (* 2. 控制流指令 *)
  | Label s -> [ s ^ ":" ]
  | Jump s -> [ Printf.sprintf "  j %s" s ]
  | BranchZ (src, label) ->
    (* 步骤: load src to t5 -> beqz t5, label *)
    let load_src = ensure_in_reg src temp_reg1 alloc_map stack_layout in
    let branch_instr = [ Printf.sprintf "  beqz %s, %s" temp_reg1 label ] in
    load_src @ branch_instr
  | BranchNZ (src, label) ->
    let load_src = ensure_in_reg src temp_reg1 alloc_map stack_layout in
    let branch_instr = [ Printf.sprintf "  bnez %s, %s" temp_reg1 label ] in
    load_src @ branch_instr
  | Branch (op, src1, src2, label) ->
    (* 步骤: load src1 to t5 -> load src2 to t6 -> b<op> t5, t6, label *)
    let load_src1 = ensure_in_reg src1 temp_reg1 alloc_map stack_layout in
    let load_src2 = ensure_in_reg src2 temp_reg2 alloc_map stack_layout in
    let branch_op =
      match op with
      | IR_Eq -> "beq"
      | IR_Neq -> "bne"
      | IR_Lt -> "blt"
      | IR_Le -> "ble"
      | IR_Gt -> "bgt"
      | IR_Ge -> "bge"
      | _ -> failwith "Branch instruction has non-comparison operator"
    in
    let branch_instr =
      [ Printf.sprintf "  %s %s, %s, %s" branch_op temp_reg1 temp_reg2 label ]
    in
    load_src1 @ load_src2 @ branch_instr
  (* 3. 函数调用相关指令 *)
  | PreCall size -> if size > 0 then [ Printf.sprintf "  addi sp, sp, -%d" size ] else []
  | Call fname -> [ Printf.sprintf "  call %s" fname ]
  | PostCall size -> if size > 0 then [ Printf.sprintf "  addi sp, sp, %d" size ] else []
  | StoreOutArg (src, offset) ->
    (* 将 src 的值加载到 t5，然后存储到 sp 相对的位置 *)
    let load_src = ensure_in_reg src temp_reg1 alloc_map stack_layout in
    let store_instr = [ Printf.sprintf "  sw %s, %d(sp)" temp_reg1 offset ] in
    load_src @ store_instr
  | Ret ->
    (* Ret本身不生成代码，它通常由Epilogue处理。
         或者我们可以生成一个跳转到函数统一返回点的指令。*)
    [ "  # Ret instruction, handled by epilogue" ]
  (* 4. 函数框架指令 *)
  | Prologue (fname, _) ->
    let s = stack_layout.total_size in
    [ ".text"
    ; ".globl " ^ fname
    ; fname ^ ":"
    ; Printf.sprintf "  addi sp, sp, -%d" s
    ; Printf.sprintf "  sw ra, %d(sp)" (s - 4)
    ; Printf.sprintf "  sw fp, %d(sp)" (s - 8)
    ; Printf.sprintf "  addi fp, sp, %d" s
    ]
  | Epilogue (fname, _) ->
    let s = stack_layout.total_size in
    [ ".L_ret_" ^ fname ^ ":" (* 统一返回点 *)
    ; Printf.sprintf "  lw fp, %d(sp)" (s - 8)
    ; Printf.sprintf "  lw ra, %d(sp)" (s - 4)
    ; Printf.sprintf "  addi sp, sp, %d" s
    ; "  ret"
    ]
  (* 如果有未处理的指令，明确地报错 *)
  | Store (_, _) ->
    failwith "Store instruction is not handled yet in final asm generation"
;;

let gen_assembly_for_function (f : func_def) (ana : analysis_result) : string list =
  (* 1. 生成使用虚拟寄存器的IR *)
  let ir_code = gen_func_ir_internal ana f in
  (* 假设这个函数返回一个函数的IR列表 *)
  (* 2. 执行寄存器分配 *)
  let alloc_map, spill_size = allocate_registers_for_function ir_code in
  (* 3. 计算最终的栈布局 *)
  (* 这里的 original_size 需要通过分析IR中的本地变量来计算，或者从旧的prologue中获取 *)
  let original_size =
    0
    (* 简化：需要一个方法来计算非溢出所需的栈空间 *)
  in
  let total_stack_size =
    let required = original_size + spill_size in
    if required mod 16 == 0 then required else required + (16 - (required mod 16))
  in
  let layout = { total_size = total_stack_size; spill_size } in
  (* 4. 遍历IR，生成最终汇编 *)
  List.concat_map (fun instr -> ir_to_asm_list_final instr alloc_map layout) ir_code
;;

(*******************************************************************
 * 4. 公共接口 (Public Interface)
 *******************************************************************)
(* This section remains unchanged *)
(* let string_of_ir (ir_instr : ir) : string =
  let op_to_str op =
    match op with
    | Imm i -> string_of_int i
    | Reg s -> s
    | Stack i -> Printf.sprintf "stack[%d]" i
  in
  let binop_to_str op =
    match op with
    | IR_Add -> "+"
    | IR_Sub -> "-"
    | IR_Mul -> "*"
    | IR_Div -> "/"
    | IR_Mod -> "%"
    | IR_Eq -> "=="
    | IR_Neq -> "!="
    | IR_Lt -> "<"
    | IR_Le -> "<="
    | IR_Gt -> ">"
    | IR_Ge -> ">="
    | IR_And -> "&&"
    | IR_Or -> "||"
  in
  match ir_instr with
  | Label s -> s ^ ":"
  | Li (dest, imm) -> Printf.sprintf "  %s = %d" (op_to_str dest) imm
  | Move (dest, src) -> Printf.sprintf "  %s = %s" (op_to_str dest) (op_to_str src)
  | Load (dest, src) -> Printf.sprintf "  %s = *%s" (op_to_str dest) (op_to_str src)
  | Store (src, dest) -> Printf.sprintf "  *%s = %s" (op_to_str dest) (op_to_str src)
  | StoreOutArg (src, offset) -> Printf.sprintf "  *(sp + %d) = %s" offset (op_to_str src)
  | Jump s -> Printf.sprintf "  j %s" s
  | PreCall size -> Printf.sprintf "  precall %d" size
  | Call s -> Printf.sprintf "  call %s" s
  | PostCall size -> Printf.sprintf "  postcall %d" size
  | Ret -> "  ret"
  | UnOp (op, dest, src) ->
    let op_str =
      match op with
      | IR_Neg -> "-"
      | IR_Not -> "!"
    in
    Printf.sprintf "  %s = %s%s" (op_to_str dest) op_str (op_to_str src)
  | BinOp (op, dest, src1, src2) ->
    Printf.sprintf
      "  %s = %s %s %s"
      (op_to_str dest)
      (op_to_str src1)
      (binop_to_str op)
      (op_to_str src2)
  | BranchZ (src, label) -> Printf.sprintf "  ifz %s j %s" (op_to_str src) label
  | BranchNZ (src, label) -> Printf.sprintf "  ifnz %s j %s" (op_to_str src) label
  | Branch (op, src1, src2, label) ->
    Printf.sprintf
      "  if %s %s %s j %s"
      (op_to_str src1)
      (binop_to_str op)
      (op_to_str src2)
      label
  | Prologue (fname, size) -> Printf.sprintf "prologue %s, %d" fname size
  | Epilogue (fname, size) -> Printf.sprintf "epilogue %s, %d" fname size
;; *)

(* let gen_program (p : program) : ir list =
  let ana = Semantic.analyze_program p in
  List.concat_map (gen_func_ir_internal ana) p
;;

let gen_assembly (ir_code : ir list) : string list =
  let current_fname = ref "" in
  let convert_ir_to_asm ir =
    (match ir with
     | Prologue (fname, _) -> current_fname := fname
     | Epilogue (fname, _) -> current_fname := fname
     | _ -> ());
    if ir = Ret
    then [ Printf.sprintf "  j .L_ret_%s" !current_fname ]
    else ir_to_asm_list_internal ir
  in
  List.concat_map convert_ir_to_asm ir_code
;;

let generate_code (p : program) : string =
  let ir = gen_program p in
  let asm_lines = gen_assembly ir in
  String.concat "\n" asm_lines
;;

let compile_source (src : string) : string =
  let lexbuf = Lexing.from_string src in
  let ast = Parser.program Lexer.token lexbuf in
  generate_code ast
;; *)
