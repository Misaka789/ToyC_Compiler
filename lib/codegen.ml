(* lib/codegen.ml *)
(* open Ast
open Semantic

(*******************************************************************
 * 1. 中间表示 (Intermediate Representation) 和环境定义
 *******************************************************************)

(**
 * 操作数类型。
 * 这是 codegen.mli 中 `type operand` 的具体实现。
 *)
type operand =
  | Imm of int          (* 立即数, e.g., 5 *)
  | Reg of string       (* 物理或虚拟寄存器, e.g., "t0", "a0" *)
  | Stack of int        (* 栈上位置 (相对于fp的偏移量), e.g., -4, -8 *)

(* * 为了在没有 .mli 文件的情况下打破模块间的类型依赖，
 * 我们在内部复制一份操作符的定义。
 *)
type ir_binop = | IR_Add | IR_Sub | IR_Mul | IR_Div | IR_Mod
                | IR_Eq | IR_Neq | IR_Lt | IR_Le | IR_Gt | IR_Ge
                | IR_And | IR_Or
type ir_unop = IR_Neg | IR_Not

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
  | Store of operand * operand
  | Branch of ir_binop * operand * operand * string
  | BranchZ of operand * string
  | BranchNZ of operand * string
  | Jump of string
  | Call of string * int
  | Ret
  | Prologue of string * int
  | Epilogue of string * int

(* 代码生成环境 (内部使用) *)
type cg_env = {
  funcs: func_sig FuncEnv.t;
  vars: int VarEnv.t;
  mutable stack_top: int;
  mutable temp_counter: int;
  mutable label_counter: int;
}

(* 创建一个新的临时寄存器名 (t0-t5) *)
let fresh_temp_reg env =
  if env.temp_counter >= 6 then failwith "Expression too complex, ran out of temporary registers";
  let reg_name = "t" ^ string_of_int env.temp_counter in
  env.temp_counter <- env.temp_counter + 1;
  Reg reg_name

(* 创建一个新的标签 *)
let fresh_label env pfx =
  let label_name = pfx ^ string_of_int env.label_counter in
  env.label_counter <- env.label_counter + 1;
  label_name

(* 在栈上为临时计算结果分配空间 *)
let alloc_temp_stack_slot env =
  env.stack_top <- env.stack_top - 4;
  Stack env.stack_top

(* 将 Ast 操作符转换为内部 IR 操作符的辅助函数 *)
let binop_from_ast_op op =
  match op with
  | Add -> IR_Add | Sub -> IR_Sub | Mul -> IR_Mul | Div -> IR_Div | Mod -> IR_Mod
  | Eq -> IR_Eq | Neq -> IR_Neq | Lt -> IR_Lt | Le -> IR_Le | Gt -> IR_Gt | Ge -> IR_Ge
  | And -> IR_And | Or -> IR_Or

let unop_from_ast_op op =
  match op with
  | Neg -> IR_Neg | Not -> IR_Not

(*******************************************************************
 * 2. 从 AST 到 IR 的转换 (内部函数)
 *******************************************************************)

let rec gen_expr_ir_internal env (e: expr) : ir list * operand =
  match e with
  | IntLiteral n ->
      let temp_reg = fresh_temp_reg env in
      [Li (temp_reg, n)], temp_reg
  | Id x ->
      let var_loc = VarEnv.find x env.vars in
      let temp_reg = fresh_temp_reg env in
      [Load (temp_reg, Stack var_loc)], temp_reg
  | Assign (x, rhs_expr) ->
      let rhs_ir, rhs_op = gen_expr_ir_internal env rhs_expr in
      let var_loc = VarEnv.find x env.vars in
      rhs_ir @ [Store (rhs_op, Stack var_loc)], rhs_op
  | UnOp (op, expr) ->
      let expr_ir, expr_op = gen_expr_ir_internal env expr in
      let dest_reg = fresh_temp_reg env in
      expr_ir @ [UnOp (unop_from_ast_op op, dest_reg, expr_op)], dest_reg
  | BinOp (op, e1, e2) ->
      (match op with
      | And ->
          let dest_reg = fresh_temp_reg env in
          let false_label = fresh_label env "L_false_" in
          let end_label = fresh_label env "L_end_" in
          let ir1, op1 = gen_expr_ir_internal env e1 in
          env.temp_counter <- 0; (* Reset for e2 *)
          let ir2, op2 = gen_expr_ir_internal env e2 in
          ir1 @ [BranchZ(op1, false_label)] @ ir2 @ [BranchZ(op2, false_label)] @
          [Li(dest_reg, 1); Jump(end_label); Label(false_label); Li(dest_reg, 0); Label(end_label)], dest_reg
      | Or ->
          let dest_reg = fresh_temp_reg env in
          let true_label = fresh_label env "L_true_" in
          let end_label = fresh_label env "L_end_" in
          let ir1, op1 = gen_expr_ir_internal env e1 in
          env.temp_counter <- 0; (* Reset for e2 *)
          let ir2, op2 = gen_expr_ir_internal env e2 in
          ir1 @ [BranchNZ(op1, true_label)] @ ir2 @ [BranchNZ(op2, true_label)] @
          [Li(dest_reg, 0); Jump(end_label); Label(true_label); Li(dest_reg, 1); Label(end_label)], dest_reg
      | _ ->
          (* Robust strategy: evaluate right, spill, evaluate left, load, compute *)
          let ir2, op2 = gen_expr_ir_internal env e2 in
          let temp_slot = alloc_temp_stack_slot env in
          let save_ir = [Store(op2, temp_slot)] in
          env.temp_counter <- 0;
          let ir1, op1 = gen_expr_ir_internal env e1 in
          let loaded_op2 = fresh_temp_reg env in
          let load_ir = [Load(loaded_op2, temp_slot)] in
          let dest_reg = fresh_temp_reg env in
          let final_op = binop_from_ast_op op in
          (match final_op with
          | IR_Eq | IR_Neq | IR_Lt | IR_Le | IR_Gt | IR_Ge ->
              let true_label = fresh_label env "L_true_" in
              let end_label = fresh_label env "L_end_" in
              ir2 @ save_ir @ ir1 @ load_ir @
              [Li(dest_reg, 0); Branch(final_op, op1, loaded_op2, true_label); Jump(end_label);
               Label(true_label); Li(dest_reg, 1); Label(end_label)], dest_reg
          | _ ->
              ir2 @ save_ir @ ir1 @ load_ir @ [BinOp(final_op, dest_reg, op1, loaded_op2)], dest_reg
          )
      )
  | Call (fname, args) ->
      let args_code_and_ops = List.map (gen_expr_ir_internal env) args in
      let args_code = List.concat_map fst args_code_and_ops in
      let arg_ops = List.map snd args_code_and_ops in
      let reg_args, stack_args =
        let rec split n lst = if n <= 0 then ([], lst) else match lst with | [] -> ([], []) | h :: t -> let (taken, rest) = split (n - 1) t in (h :: taken, rest)
        in split 8 arg_ops
      in
      let reg_passing_ir = List.mapi (fun i op -> Move (Reg ("a" ^ string_of_int i), op)) reg_args in
      let stack_passing_ir = List.mapi (fun i op -> Store (op, Stack (i * -4))) (List.rev stack_args) in
      let num_stack_args = List.length stack_args in
      let ret_reg = Reg "a0" in
      let temp_ret_reg = fresh_temp_reg env in
      let call_ir = [Call (fname, num_stack_args); Move (temp_ret_reg, ret_reg)] in
      args_code @ stack_passing_ir @ reg_passing_ir @ call_ir, temp_ret_reg
 (* | _ -> failwith "Unsupported expression type in codegen"*)

and gen_stmt_ir_internal env ?break_lbl ?cont_lbl (s: stmt) : ir list =
  env.temp_counter <- 0;
  match s with
  | Expr e -> fst (gen_expr_ir_internal env e)
  | Return None -> [Ret]
  | Return (Some e) ->
      let ir, op = gen_expr_ir_internal env e in
      ir @ [Move (Reg "a0", op); Ret]
  | VarDecl (id, init_e) ->
      let ir, op = gen_expr_ir_internal env init_e in
      let var_loc = VarEnv.find id env.vars in
      ir @ [Store (op, Stack var_loc)]
  | Block stmts -> List.concat_map (gen_stmt_ir_internal env ?break_lbl ?cont_lbl) stmts
  | If (cond, then_s, else_s_opt) ->
      let cond_ir, cond_op = gen_expr_ir_internal env cond in
      let else_label = fresh_label env "L_else_" in
      let end_label = fresh_label env "L_end_" in
      let then_ir = gen_stmt_ir_internal env ?break_lbl ?cont_lbl then_s in
      (match else_s_opt with
      | None ->
          cond_ir @ [BranchZ (cond_op, end_label)] @ then_ir @ [Label end_label]
      | Some else_s ->
          let else_ir = gen_stmt_ir_internal env ?break_lbl ?cont_lbl else_s in
          cond_ir @ [BranchZ (cond_op, else_label)] @ then_ir @ [Jump end_label; Label else_label] @ else_ir @ [Label end_label]
      )
  | While (cond, body) ->
      let start_label = fresh_label env "L_while_start_" in
      let end_label = fresh_label env "L_while_end_" in
      let cond_ir, cond_op = gen_expr_ir_internal env cond in
      let body_ir = gen_stmt_ir_internal env ~break_lbl:end_label ~cont_lbl:start_label body in
      [Label start_label] @ cond_ir @ [BranchZ (cond_op, end_label)] @ body_ir @ [Jump start_label; Label end_label]
  | Break -> (match break_lbl with Some lbl -> [Jump lbl] | None -> failwith "break statement not within a loop")
  | Continue -> (match cont_lbl with Some lbl -> [Jump lbl] | None -> failwith "continue statement not within a loop")

let gen_func_ir_internal (ana: analysis_result) (f: func_def) : ir list =
  let locals_map = List.assoc f.fname ana.local_vars in
  let stack_offset = ref (-8) in
  let assign_offset () = stack_offset := !stack_offset - 4; !stack_offset in
  let params_with_offsets =
    List.mapi (fun i name ->
      if i < 8 then (name, assign_offset (), Some (Reg ("a" ^ string_of_int i)))
      else (name, 8 + (i - 8) * 4, None)
    ) f.params
  in
  let locals_with_offsets = VarEnv.fold (fun name _ acc -> (name, assign_offset (), None) :: acc) locals_map [] in
  let var_map = List.fold_left (fun acc (name, offset, _) -> VarEnv.add name offset acc) VarEnv.empty (params_with_offsets @ locals_with_offsets) in
  let env = {
    funcs = ana.global_funcs; vars = var_map; stack_top = !stack_offset;
    temp_counter = 0; label_counter = 0;
  } in
  let body_ir = List.concat_map (gen_stmt_ir_internal env) f.body in
  let params_save_ir =
    List.filter_map (function (_, offset, Some reg) -> Some (Store (reg, Stack offset)) | _ -> None) params_with_offsets
  in
  let required_stack = abs env.stack_top + 8 in
  let stack_size = if required_stack mod 16 == 0 then required_stack else required_stack + (16 - required_stack mod 16) in
  [Prologue (f.fname, stack_size)] @ params_save_ir @ body_ir @ [Epilogue (f.fname, stack_size)]

(*******************************************************************
 * 3. 从 IR 到 RISC-V 汇编的转换 (内部函数)
 *******************************************************************)

let ir_to_asm_list_internal (ir_instr: ir) : string list =
  let op_to_str op = match op with
    | Imm i -> string_of_int i
    | Reg s -> s
    | Stack i -> Printf.sprintf "%d(fp)" i
  in
  match ir_instr with
  | Label s -> [s ^ ":"]
  | Li (dest, imm) -> [Printf.sprintf "  li %s, %d" (op_to_str dest) imm]
  | Move (dest, src) -> [Printf.sprintf "  mv %s, %s" (op_to_str dest) (op_to_str src)]
  | Load (dest, src) -> [Printf.sprintf "  lw %s, %s" (op_to_str dest) (op_to_str src)]
  | Store (src, Stack i) when i < 0 -> [Printf.sprintf "  sw %s, %d(fp)" (op_to_str src) i]
  | Store (src, Stack i) -> [Printf.sprintf "  sw %s, %d(sp)" (op_to_str src) i]
  | Store (src, dest) -> [Printf.sprintf "  sw %s, %s" (op_to_str src) (op_to_str dest)]
  | Jump s -> [Printf.sprintf "  j %s" s]
  | Ret -> failwith "Ret should not be directly converted, it's handled by Epilogue"
  | Call (s, num_stack_args) ->
      let stack_space = num_stack_args * 4 in
      if stack_space > 0 then
        [Printf.sprintf "  addi sp, sp, -%d" stack_space;
         Printf.sprintf "  call %s" s;
         Printf.sprintf "  addi sp, sp, %d" stack_space]
      else [Printf.sprintf "  call %s" s]
  | UnOp (op, dest, src) ->
      let op_str = match op with IR_Neg -> "neg" | IR_Not -> "seqz" in
      [Printf.sprintf "  %s %s, %s" op_str (op_to_str dest) (op_to_str src)]
  | BinOp (op, dest, src1, src2) ->
      let op_str = match op with | IR_Add -> "add" | IR_Sub -> "sub" | IR_Mul -> "mul" | IR_Div -> "div" | IR_Mod -> "rem" | _ -> failwith "Invalid op" in
      [Printf.sprintf "  %s %s, %s, %s" op_str (op_to_str dest) (op_to_str src1) (op_to_str src2)]
  | BranchZ (src, label) -> [Printf.sprintf "  beqz %s, %s" (op_to_str src) label]
  | BranchNZ (src, label) -> [Printf.sprintf "  bnez %s, %s" (op_to_str src) label]
  | Branch (op, src1, src2, label) ->
      let branch_op_str = match op with | IR_Eq -> "beq" | IR_Neq -> "bne" | IR_Lt -> "blt" | IR_Le -> "ble" | IR_Gt -> "bgt" | IR_Ge -> "bge" | _ -> failwith "Invalid op" in
      (match src1, src2 with
       | Reg _, Reg _ -> [Printf.sprintf "  %s %s, %s, %s" branch_op_str (op_to_str src1) (op_to_str src2) label]
       | Reg r, Imm i -> [Printf.sprintf "  li t6, %d" i; Printf.sprintf "  %s %s, t6, %s" branch_op_str r label]
       | Imm i, Reg r -> [Printf.sprintf "  li t6, %d" i; Printf.sprintf "  %s t6, %s, %s" branch_op_str r label]
       | _ -> failwith "Invalid operands for branch")
  | Prologue (fname, stack_size) ->
      [ "  .text"; "  .globl " ^ fname; fname ^ ":";
        Printf.sprintf "  addi sp, sp, -%d" stack_size;
        Printf.sprintf "  sw ra, %d(sp)" (stack_size - 4);
        Printf.sprintf "  sw fp, %d(sp)" (stack_size - 8);
        Printf.sprintf "  addi fp, sp, %d" stack_size; ]
  | Epilogue (fname, stack_size) ->
      [ ".L_ret_" ^ fname ^ ":";
        Printf.sprintf "  lw fp, %d(sp)" (stack_size - 8);
        Printf.sprintf "  lw ra, %d(sp)" (stack_size - 4);
        Printf.sprintf "  addi sp, sp, %d" stack_size;
        "  ret"; ]

(*******************************************************************
 * 4. 公共接口 (Public Interface)
 *******************************************************************)

let string_of_ir (ir_instr: ir) : string =
  let op_to_str op = match op with
    | Imm i -> string_of_int i
    | Reg s -> s
    | Stack i -> Printf.sprintf "stack[%d]" i
  in
  let binop_to_str op = match op with
    | IR_Add -> "+" | IR_Sub -> "-" | IR_Mul -> "*" | IR_Div -> "/" | IR_Mod -> "%"
    | IR_Eq -> "==" | IR_Neq -> "!=" | IR_Lt -> "<" | IR_Le -> "<=" | IR_Gt -> ">" | IR_Ge -> ">="
    | IR_And -> "&&" | IR_Or -> "||"
  in
  match ir_instr with
  | Label s -> s ^ ":"
  | Li (dest, imm) -> Printf.sprintf "  %s = %d" (op_to_str dest) imm
  | Move (dest, src) -> Printf.sprintf "  %s = %s" (op_to_str dest) (op_to_str src)
  | Load (dest, src) -> Printf.sprintf "  %s = *%s" (op_to_str dest) (op_to_str src)
  | Store (src, dest) -> Printf.sprintf "  *%s = %s" (op_to_str dest) (op_to_str src)
  | Jump s -> Printf.sprintf "  j %s" s
  | Ret -> "  ret"
  | Call (s, n) -> Printf.sprintf "  call %s, %d" s n
  | UnOp (op, dest, src) ->
      let op_str = match op with IR_Neg -> "-" | IR_Not -> "!" in
      Printf.sprintf "  %s = %s%s" (op_to_str dest) op_str (op_to_str src)
  | BinOp (op, dest, src1, src2) ->
      Printf.sprintf "  %s = %s %s %s" (op_to_str dest) (op_to_str src1) (binop_to_str op) (op_to_str src2)
  | BranchZ (src, label) -> Printf.sprintf "  ifz %s j %s" (op_to_str src) label
  | BranchNZ (src, label) -> Printf.sprintf "  ifnz %s j %s" (op_to_str src) label
  | Branch (op, src1, src2, label) ->
      Printf.sprintf "  if %s %s %s j %s" (op_to_str src1) (binop_to_str op) (op_to_str src2) label
  | Prologue (fname, size) -> Printf.sprintf "prologue %s, %d" fname size
  | Epilogue (fname, size) -> Printf.sprintf "epilogue %s, %d" fname size

let gen_program (p: program) : ir list =
  let ana = Semantic.analyze_program p in
  List.concat_map (gen_func_ir_internal ana) p

let gen_assembly (ir_code: ir list) : string list =
  let current_fname = ref "" in
  let convert_ir_to_asm ir =
    (match ir with
    | Prologue (fname, _) -> current_fname := fname
    | Epilogue (fname, _) -> current_fname := fname
    | _ -> ());
    if ir = Ret then [Printf.sprintf "  j .L_ret_%s" !current_fname]
    else ir_to_asm_list_internal ir
  in
  List.concat_map convert_ir_to_asm ir_code

let generate_code (p: program) : string =
  let ir = gen_program p in
  let asm_lines = gen_assembly ir in
  String.concat "\n" asm_lines

let compile_source (src: string) : string =
  let lexbuf = Lexing.from_string src in
  let ast = Parser.program Lexer.token lexbuf in
  generate_code ast *)

(*使用Sethi_Ullman算法来解决寄存器溢出的问题*)
(* lib/codegen.ml *)
open Ast
open Semantic

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
  | Store of operand * operand
  | Branch of ir_binop * operand * operand * string
  | BranchZ of operand * string
  | BranchNZ of operand * string
  | Jump of string
  | Call of string * int
  | Ret
  | Prologue of string * int
  | Epilogue of string * int

(* 代码生成环境 (内部使用) *)
type cg_env =
  { funcs : func_sig FuncEnv.t
  ; vars : int VarEnv.t list (* A stack of scopes，变成list存储不同作用域 *)
  ; stack_top : int ref
  ; mutable temp_counter : int
  ; mutable label_counter : int
  ; current_function_name : string (* NEW: Store the current function's name，用于label命名 *)
  ; mutable used_s_regs : string list (*使用过的寄存器列表*)
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

(*可用寄存器池*)
let available_regs =
  [ "t0"
  ; "t1"
  ; "t2"
  ; "t3"
  ; "t4"
  ; "t5"
  ; "t6"
  ; "s0"
  ; "s1"
  ; "s2"
  ; "s3"
  ; "s4"
  ; "s5"
  ; "s6"
  ; "s7"
  ; "s8"
  ; "s9"
  ; "s10"
  ; "s11"
  ]
;;

(* 创建一个新的临时寄存器名 (t0-t5) *)
(* let fresh_temp_reg env =
  if env.temp_counter >= 6 then failwith "Expression too complex, ran out of temporary registers";
let reg_name = "t" ^ string_of_int env.temp_counter in
  env.temp_counter <- env.temp_counter + 1;
Reg reg_name *)

(*修改 ：移除数量检查，可以使用无限多虚拟寄存器，并且这里需要追踪寄存器的使用*)
let fresh_temp_reg env =
  if env.temp_counter >= List.length available_regs
  then
    failwith
      "Expression too complex, ran out of all available temporary and saved registers";
  let reg_name = List.nth available_regs env.temp_counter in
  env.temp_counter <- env.temp_counter + 1;
  (* 如果我们分配了一个 's' 寄存器，并且是第一次使用它，就记录下来 *)
  if String.starts_with ~prefix:"s" reg_name && not (List.mem reg_name env.used_s_regs)
  then env.used_s_regs <- reg_name :: env.used_s_regs;
  Reg reg_name
;;

(*
   * 为了避免在递归中重复计算同一个子树的寄存器需求，我们使用一个Map作为缓存。
 * OCaml的Map模块需要一个比较函数来对key进行排序。
 * 我们不能直接用 `expr` 作为key，因为它们没有默认的比较函数。
 * 最简单的解决方案是在代码生成前，给AST的每个节点分配一个唯一的ID。
 * 如果不想修改AST，我们可以用一个更取巧的方式，但这会使代码更复杂。
 *
 * 为了课程设计简单起见，我们这里采用一个不带缓存的版本。
 * 对于中等复杂的表达式，性能影响不大。
*)

(*添加 ：计算子树需要的最大寄存器的数量 *)
let rec calculate_regs_needed (e : expr) : int =
  match e with
  (* 叶子节点: 只需要1个寄存器来加载它们的值 *)
  | IntLiteral _ | Id _ -> 1
  (* 一元操作: 需求等于其子节点的需求 *)
  | UnOp (_, sub_e) -> calculate_regs_needed sub_e
  (* 赋值操作: 需求等于其右侧表达式的需求 *)
  | Assign (_, rhs_e) -> calculate_regs_needed rhs_e
  (* 函数调用: 至少需要1个寄存器来持有返回值。
     参数的计算会独立进行，所以我们只关心最终调用。这是一个简化。*)
  | Call (_, args) ->
    let arg_regs = List.map calculate_regs_needed args in
    (* 峰值需求是所有参数计算需求中的最大值，或者至少是1 *)
    List.fold_left max 0 arg_regs + 1
  (* 二元操作: 这是算法的核心 *)
  | BinOp (_, e1, e2) ->
    let n1 = calculate_regs_needed e1 in
    let n2 = calculate_regs_needed e2 in
    if n1 = n2
    then n1 + 1 (* 如果两边需求相同，需要 n1 个寄存器计算一边，然后用第 n1+1 个寄存器来持有结果，同时计算另一边 *)
    else max n1 n2 (* 如果需求不同，先计算需求大的那一边，其结果可以被持有，同时在相同的寄存器集上计算另一边 *)
;;

(* 创建一个新的标签，加上函数名前缀 *)
let fresh_label env pfx =
  let label_name =
    Printf.sprintf "%s_%s%d" env.current_function_name pfx env.label_counter
  in
  env.label_counter <- env.label_counter + 1;
  label_name
;;

(* 在栈上为临时计算结果分配空间 *)
let alloc_temp_stack_slot env =
  env.stack_top := !(env.stack_top) - 4;
  Stack !(env.stack_top)
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

(*******************************************************************
 * 2. 从 AST 到 IR 的转换 (内部函数)
 *******************************************************************)

let rec gen_expr_ir_internal env (e : expr) : ir list * operand =
  match e with
  | IntLiteral n ->
    let temp_reg = fresh_temp_reg env in
    [ Li (temp_reg, n) ], temp_reg
  | Id x ->
    let var_loc = find_var_offset env x in
    let temp_reg = fresh_temp_reg env in
    [ Load (temp_reg, Stack var_loc) ], temp_reg
  | Assign (x, rhs_expr) ->
    let rhs_ir, rhs_op = gen_expr_ir_internal env rhs_expr in
    let var_loc = find_var_offset env x in
    rhs_ir @ [ Store (rhs_op, Stack var_loc) ], rhs_op
  | UnOp (op, expr) ->
    let expr_ir, expr_op = gen_expr_ir_internal env expr in
    let dest_reg = fresh_temp_reg env in
    expr_ir @ [ UnOp (unop_from_ast_op op, dest_reg, expr_op) ], dest_reg
  | BinOp (op, e1, e2) ->
    (match op with
     | And ->
       let dest_reg = fresh_temp_reg env in
       let false_label = fresh_label env "L_false_" in
       let end_label = fresh_label env "L_end_" in
       let ir1, op1 = gen_expr_ir_internal env e1 in
       env.temp_counter <- 0;
       (* Reset for e2 *)
       let ir2, op2 = gen_expr_ir_internal env e2 in
       ( ir1
         @ [ BranchZ (op1, false_label) ]
         @ ir2
         @ [ BranchZ (op2, false_label) ]
         @ [ Li (dest_reg, 1)
           ; Jump end_label
           ; Label false_label
           ; Li (dest_reg, 0)
           ; Label end_label
           ]
       , dest_reg )
     | Or ->
       let dest_reg = fresh_temp_reg env in
       let true_label = fresh_label env "L_true_" in
       let end_label = fresh_label env "L_end_" in
       let ir1, op1 = gen_expr_ir_internal env e1 in
       env.temp_counter <- 0;
       (* Reset for e2 *)
       let ir2, op2 = gen_expr_ir_internal env e2 in
       ( ir1
         @ [ BranchNZ (op1, true_label) ]
         @ ir2
         @ [ BranchNZ (op2, true_label) ]
         @ [ Li (dest_reg, 0)
           ; Jump end_label
           ; Label true_label
           ; Li (dest_reg, 1)
           ; Label end_label
           ]
       , dest_reg )
       (* | _ ->
          (* Robust strategy: evaluate right, spill, evaluate left, load, compute *)
          let ir2, op2 = gen_expr_ir_internal env e2 in
          let temp_slot = alloc_temp_stack_slot env in
          let save_ir = [Store(op2, temp_slot)] in
          env.temp_counter <- 0;
          let ir1, op1 = gen_expr_ir_internal env e1 in
          let loaded_op2 = fresh_temp_reg env in
          let load_ir = [Load(loaded_op2, temp_slot)] in
          let dest_reg = fresh_temp_reg env in
          let final_op = binop_from_ast_op op in
          (match final_op with
          | IR_Eq | IR_Neq | IR_Lt | IR_Le | IR_Gt |  IR_Ge ->
              let true_label = fresh_label env "L_true_" in
              let end_label = fresh_label env "L_end_" in
              ir2 @ save_ir @ ir1 @ load_ir @
              [Li(dest_reg, 0); Branch(final_op, op1, loaded_op2, true_label); Jump(end_label);
               Label(true_label); Li(dest_reg, 1); Label(end_label)], dest_reg
          | _ ->
              ir2 @ save_ir @ ir1 @ load_ir @ [BinOp(final_op, dest_reg, op1, loaded_op2)], dest_reg
          )
      ) *)
       (* [MODIFIED] 使用 Sethi-Ullman 算法优化二元运算 *)
     | _ ->
       let n1 = calculate_regs_needed e1 in
       let n2 = calculate_regs_needed e2 in
       (* 决定求值顺序：总是先计算需要更多寄存器的子表达式 *)
       let first_expr, second_expr, is_swapped =
         if n1 < n2 then e2, e1, true (* 先算e2 *) else e1, e2, false
         (* 先算e1 *)
       in
       (* 1. 为更复杂的 first_expr 生成代码 *)
       let ir_first, op_first = gen_expr_ir_internal env first_expr in
       (* 2. 将其结果溢出到栈上 *)
       let temp_slot = alloc_temp_stack_slot env in
       let save_ir = [ Store (op_first, temp_slot) ] in
       (* 3. 重置临时寄存器计数器，为计算第二个表达式做准备 *)
       env.temp_counter <- 0;
       (* 4. 为更简单的 second_expr 生成代码 *)
       let ir_second, op_second = gen_expr_ir_internal env second_expr in
       (* 5. 从栈上加载第一个表达式的结果 *)
       let loaded_op_first = fresh_temp_reg env in
       let load_ir = [ Load (loaded_op_first, temp_slot) ] in
       (* 6. 准备最终操作的操作数，注意要根据是否交换过顺序来放置 *)
       let final_op1, final_op2 =
         if is_swapped then op_second, loaded_op_first else loaded_op_first, op_second
       in
       (* 7. 执行最终操作 *)
       let dest_reg = fresh_temp_reg env in
       let final_op = binop_from_ast_op op in
       let combined_ir = ir_first @ save_ir @ ir_second @ load_ir in
       (match final_op with
        | IR_Eq | IR_Neq | IR_Lt | IR_Le | IR_Gt | IR_Ge ->
          let true_label = fresh_label env "L_true_" in
          let end_label = fresh_label env "L_end_" in
          ( combined_ir
            @ [ Li (dest_reg, 0)
              ; Branch (final_op, final_op1, final_op2, true_label)
              ; Jump end_label
              ; Label true_label
              ; Li (dest_reg, 1)
              ; Label end_label
              ]
          , dest_reg )
        | _ ->
          combined_ir @ [ BinOp (final_op, dest_reg, final_op1, final_op2) ], dest_reg))
    (* | Call (fname, args) ->
    let args_code_and_ops = List.map (gen_expr_ir_internal env) args in
    let args_code = List.concat_map fst args_code_and_ops in
    let arg_ops = List.map snd args_code_and_ops in
    let reg_args, stack_args =
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
      split 8 arg_ops
    in
    let reg_passing_ir =
      List.mapi (fun i op -> Move (Reg ("a" ^ string_of_int i), op)) reg_args
    in
    let stack_passing_ir =
      List.mapi (fun i op -> Store (op, Stack (i * -4))) (List.rev stack_args)
    in
    let num_stack_args = List.length stack_args in
    let ret_reg = Reg "a0" in
    let temp_ret_reg = fresh_temp_reg env in
    let call_ir = [ Call (fname, num_stack_args); Move (temp_ret_reg, ret_reg) ] in
    args_code @ stack_passing_ir @ reg_passing_ir @ call_ir, temp_ret_reg *)
  | Call (fname, args) ->
    (* [修改] 采用健壮的、逐个处理参数的策略 *)
    let num_args = List.length args in
    let arg_dests =
      List.mapi
        (fun i _ -> if i < 8 then Reg ("a" ^ string_of_int i) else Stack ((i - 8) * 4))
        args
    in
    let args_setup_ir, _ =
      List.fold_right
        (fun (arg_expr, dest) (acc_ir, temp_counter_base) ->
           env.temp_counter <- temp_counter_base;
           let arg_ir, arg_op = gen_expr_ir_internal env arg_expr in
           let move_ir =
             match dest with
             | Reg _ -> [ Move (dest, arg_op) ]
             | Stack offset -> [ Store (arg_op, Stack offset) ]
             | _ -> failwith "Invalid argument destination"
           in
           acc_ir @ arg_ir @ move_ir, temp_counter_base)
        (List.combine args arg_dests)
        ([], 0)
    in
    let num_stack_args = max 0 (num_args - 8) in
    let ret_reg = Reg "a0" in
    env.temp_counter <- 0;
    let temp_ret_reg = fresh_temp_reg env in
    let call_ir = [ Call (fname, num_stack_args); Move (temp_ret_reg, ret_reg) ] in
    args_setup_ir @ call_ir, temp_ret_reg
;;

(* _ -> failwith "Unsupported expression type in codegen" *)

let rec gen_stmt_ir_internal (env : cg_env) ?break_lbl ?cont_lbl (s : stmt)
  : ir list * cg_env
  =
  env.temp_counter <- 0;
  (* MODIFIED: Do not create a copy of the environment.  *)
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
         let offset = if i < 8 then !param_offset else 8 + ((i - 8) * 4) in
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
    ; temp_counter = 0
    ; label_counter = 0
    ; current_function_name = f.fname (* INITIALIZE HERE *)
    ; used_s_regs = [] (*初始话为空*)
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
  (* [新增] 生成保存/恢复 s 寄存器的IR *)
  let s_regs_to_save = final_env.used_s_regs in
  let save_s_regs_ir =
    List.map
      (fun reg_name ->
         final_env.stack_top := !(final_env.stack_top) - 4;
         Store (Reg reg_name, Stack !(final_env.stack_top)))
      s_regs_to_save
  in
  let restore_s_regs_ir =
    List.mapi
      (fun i reg_name ->
         let offset = !(final_env.stack_top) + (i * 4) in
         Load (Reg reg_name, Stack offset))
      (List.rev s_regs_to_save)
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
  @ save_s_regs_ir
  (* [添加一行] *)
  @ params_save_ir
  @ body_ir
  @ restore_s_regs_ir
  @
  (* [添加一行] *)
  [ Epilogue (f.fname, stack_size) ]
;;

(*******************************************************************
 * 3. 从 IR 到 RISC-V 汇编的转换 (内部函数)
 *******************************************************************)
(* This section remains unchanged *)
let ir_to_asm_list_internal (ir_instr : ir) : string list =
  let op_to_str op =
    match op with
    | Imm i -> string_of_int i
    | Reg s -> s
    | Stack i -> Printf.sprintf "%d(fp)" i
  in
  match ir_instr with
  | Label s -> [ s ^ ":" ]
  | Li (dest, imm) -> [ Printf.sprintf "  li %s, %d" (op_to_str dest) imm ]
  | Move (dest, src) -> [ Printf.sprintf "  mv %s, %s" (op_to_str dest) (op_to_str src) ]
  | Load (dest, src) -> [ Printf.sprintf "  lw %s, %s" (op_to_str dest) (op_to_str src) ]
  | Store (src, Stack i) when i < 0 ->
    [ Printf.sprintf "  sw %s, %d(fp)" (op_to_str src) i ]
  | Store (src, Stack i) -> [ Printf.sprintf "  sw %s, %d(sp)" (op_to_str src) i ]
  | Store (src, dest) -> [ Printf.sprintf "  sw %s, %s" (op_to_str src) (op_to_str dest) ]
  | Jump s -> [ Printf.sprintf "  j %s" s ]
  | Ret -> failwith "Ret should not be directly converted, it's handled by Epilogue"
  | Call (s, num_stack_args) ->
    let stack_space = num_stack_args * 4 in
    if stack_space > 0
    then
      [ Printf.sprintf "  addi sp, sp, -%d" stack_space
      ; Printf.sprintf "  call %s" s
      ; Printf.sprintf "  addi sp, sp, %d" stack_space
      ]
    else [ Printf.sprintf "  call %s" s ]
  | UnOp (op, dest, src) ->
    let op_str =
      match op with
      | IR_Neg -> "neg"
      | IR_Not -> "seqz"
    in
    [ Printf.sprintf "  %s %s, %s" op_str (op_to_str dest) (op_to_str src) ]
  | BinOp (op, dest, src1, src2) ->
    let op_str =
      match op with
      | IR_Add -> "add"
      | IR_Sub -> "sub"
      | IR_Mul -> "mul"
      | IR_Div -> "div"
      | IR_Mod -> "rem"
      | _ -> failwith "Invalid op"
    in
    [ Printf.sprintf
        "  %s %s, %s, %s"
        op_str
        (op_to_str dest)
        (op_to_str src1)
        (op_to_str src2)
    ]
  | BranchZ (src, label) -> [ Printf.sprintf "  beqz %s, %s" (op_to_str src) label ]
  | BranchNZ (src, label) -> [ Printf.sprintf "  bnez %s, %s" (op_to_str src) label ]
  | Branch (op, src1, src2, label) ->
    let branch_op_str =
      match op with
      | IR_Eq -> "beq"
      | IR_Neq -> "bne"
      | IR_Lt -> "blt"
      | IR_Le -> "ble"
      | IR_Gt -> "bgt"
      | IR_Ge -> "bge"
      | _ -> failwith "Invalid op"
    in
    (match src1, src2 with
     | Reg _, Reg _ ->
       [ Printf.sprintf
           "  %s %s, %s, %s"
           branch_op_str
           (op_to_str src1)
           (op_to_str src2)
           label
       ]
     | Reg r, Imm i ->
       [ Printf.sprintf "  li t6, %d" i
       ; Printf.sprintf "  %s %s, t6, %s" branch_op_str r label
       ]
     | Imm i, Reg r ->
       [ Printf.sprintf "  li t6, %d" i
       ; Printf.sprintf "  %s t6, %s, %s" branch_op_str r label
       ]
     | _ -> failwith "Invalid operands for branch")
  | Prologue (fname, stack_size) ->
    [ "  .text"
    ; "  .globl " ^ fname
    ; fname ^ ":"
    ; Printf.sprintf "  addi sp, sp, -%d" stack_size
    ; Printf.sprintf "  sw ra, %d(sp)" (stack_size - 4)
    ; Printf.sprintf "  sw fp, %d(sp)" (stack_size - 8)
    ; Printf.sprintf "  addi fp, sp, %d" stack_size
    ]
  | Epilogue (fname, stack_size) ->
    [ ".L_ret_" ^ fname ^ ":"
    ; Printf.sprintf "  lw fp, %d(sp)" (stack_size - 8)
    ; Printf.sprintf "  lw ra, %d(sp)" (stack_size - 4)
    ; Printf.sprintf "  addi sp, sp, %d" stack_size
    ; "  ret"
    ]
;;

(*******************************************************************
 * 4. 公共接口 (Public Interface)
 *******************************************************************)
(* This section remains unchanged *)
let string_of_ir (ir_instr : ir) : string =
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
  | Jump s -> Printf.sprintf "  j %s" s
  | Ret -> "  ret"
  | Call (s, n) -> Printf.sprintf "  call %s, %d" s n
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
;;

let gen_program (p : program) : ir list =
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
;;
