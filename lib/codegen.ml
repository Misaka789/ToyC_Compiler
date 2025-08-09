
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
  | Store of operand * operand       (* This now ALWAYS means fp-relative store *)
  | StoreOutArg of operand * int   (* NEW: Explicitly for sp-relative outgoing args *)
  | Branch of ir_binop * operand * operand * string
  | BranchZ of operand * string
  | BranchNZ of operand * string
  | Jump of string
  | PreCall of int        (* MODIFIED: Was Call of string * int *)
  | Call of string
  | PostCall of int
  | Ret
  | Prologue of string * int
  | Epilogue of string * int

(* 代码生成环境 (内部使用) *)
type cg_env = {
  funcs: func_sig FuncEnv.t;
  vars: int VarEnv.t list; (* A stack of scopes，变成list存储不同作用域 *)
  stack_top: int ref;      
   temp_counter: int ref;        (* MODIFIED *)
  label_counter: int ref;       (* MODIFIED *)
  current_function_name: string; (* NEW: Store the current function's name，用于label命名 *)
}

(* 在作用域栈中查找变量 *)
let find_var_offset (env: cg_env) (name: string) : int =
  let rec find_in_scopes scopes =
    match scopes with
    | [] -> failwith ("Undeclared variable: " ^ name)
    | current_scope :: outer_scopes ->
        match VarEnv.find_opt name current_scope with
        | Some offset -> offset
        | None -> find_in_scopes outer_scopes
  in
  find_in_scopes env.vars

(* 在栈上为临时计算结果分配空间 *)
let alloc_temp_stack_slot env =
  env.stack_top := !(env.stack_top) - 4;
Stack !(env.stack_top)

(* 创建一个新的临时操作数。优先使用寄存器 (t0-t5)，用尽后在栈上分配空间 *)
let fresh_temp env = (* Renamed from fresh_temp for clarity *)
  if !(env.temp_counter) < 6 then (
    let reg_name = "t" ^ string_of_int !(env.temp_counter) in
    env.temp_counter := !(env.temp_counter) + 1;
    Reg reg_name
  ) else (
    alloc_temp_stack_slot env
  )

(* 创建一个新的标签 *)
let fresh_label env pfx =
  let label_name = Printf.sprintf "%s_%s%d" env.current_function_name pfx !(env.label_counter) in
  env.label_counter := !(env.label_counter) + 1;
  label_name


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
      let temp_reg = fresh_temp env in
      [Li (temp_reg, n)], temp_reg
  | Id x ->
      let var_loc = find_var_offset env x in
      let temp_reg = fresh_temp env in
      [Load (temp_reg, Stack var_loc)], temp_reg
  | Assign (x, rhs_expr) ->
      let rhs_ir, rhs_op = gen_expr_ir_internal env rhs_expr in
      let var_loc = find_var_offset env x in
      rhs_ir @ [Store (rhs_op, Stack var_loc)], rhs_op
  | UnOp (op, expr) ->
      let expr_ir, expr_op = gen_expr_ir_internal env expr in
      let dest_reg = fresh_temp env in
      expr_ir @ [UnOp (unop_from_ast_op op, dest_reg, expr_op)], dest_reg
  | BinOp (op, e1, e2) ->
      (match op with
      | And ->
          let dest_op = fresh_temp env in
          let false_label = fresh_label env "L_false_" in
          let end_label = fresh_label env "L_end_" in
          let ir1, op1 = gen_expr_ir_internal env e1 in
          env.temp_counter := 0;
          let ir2, op2 = gen_expr_ir_internal env e2 in
          ir1 @ [BranchZ(op1, false_label)] @ ir2 @ [BranchZ(op2, false_label)] @
          [Li(dest_op, 1); Jump(end_label); Label(false_label); Li(dest_op, 0); Label(end_label)], dest_op
      | Or ->
          let dest_op = fresh_temp env in
          let true_label = fresh_label env "L_true_" in
          let end_label = fresh_label env "L_end_" in
          let ir1, op1 = gen_expr_ir_internal env e1 in
          env.temp_counter := 0;
          let ir2, op2 = gen_expr_ir_internal env e2 in
          ir1 @ [BranchNZ(op1, true_label)] @ ir2 @ [BranchNZ(op2, true_label)] @
          [Li(dest_op, 0); Jump(end_label); Label(true_label); Li(dest_op, 1); Label(end_label)], dest_op
      
      (* The robust "Spill-and-Reload" strategy, but this time with a correct assembler *)
      (* | _ ->
          (* This order of evaluation (e1 then e2) is more conventional *)
          let ir1, op1 = gen_expr_ir_internal env e1 in
          let temp_slot_for_op1 = alloc_temp_stack_slot env in
          let save_ir = [Store(op1, temp_slot_for_op1)] in
          
          env.temp_counter := 0; (* Reset temps for the other side *)
          
          let ir2, op2 = gen_expr_ir_internal env e2 in
          
          let loaded_op1 = fresh_temp env in
          let load_ir = [Load(loaded_op1, temp_slot_for_op1)] in
          
          let dest_op = fresh_temp env in
          let final_op = binop_from_ast_op op in
          
          let full_ir = ir1 @ save_ir @ ir2 @ load_ir in
          
          (match final_op with
          | IR_Eq | IR_Neq | IR_Lt | IR_Le | IR_Gt | IR_Ge ->
              let true_label = fresh_label env "L_true_" in
              let end_label = fresh_label env "L_end_" in
              full_ir @
              [Li(dest_op, 0);
               Branch(final_op, loaded_op1, op2, true_label);
               Jump(end_label);
               Label(true_label);
               Li(dest_op, 1);
               Label(end_label)], dest_op
          | _ ->
              full_ir @ [BinOp(final_op, dest_op, loaded_op1, op2)], dest_op
          ) *)
           | _ ->
    (* 采用更简洁、鲁棒的顺序求值策略 *)
    let ir1, op1 = gen_expr_ir_internal env e1 in
    let ir2, op2 = gen_expr_ir_internal env e2 in
    
    let dest_op = fresh_temp env in
    let final_op = binop_from_ast_op op in
    
    let full_ir = ir1 @ ir2 in (* 直接连接两个操作数的IR *)
    
    (match final_op with
    | IR_Eq | IR_Neq | IR_Lt | IR_Le | IR_Gt | IR_Ge ->
        let true_label = fresh_label env "L_true_" in
        let end_label = fresh_label env "L_end_" in
        full_ir @
        [Li(dest_op, 0);
         Branch(final_op, op1, op2, true_label); (* 直接使用 op1 和 op2 *)
         Jump(end_label);
         Label(true_label);
         Li(dest_op, 1);
         Label(end_label)], dest_op
    | _ ->
        full_ir @ [BinOp(final_op, dest_op, op1, op2)], dest_op (* 直接使用 op1 和 op2 *)
    )
      )
| Call (fname, args) ->
      (* 步骤 1: 依次求值并立即溢出每个参数的结果到调用者的栈帧上 (fp-relative) *)
      let (eval_ir, arg_spill_locs_rev) =
        List.fold_left (fun (acc_ir, acc_locs) arg_expr ->
          env.temp_counter := 0;
          let arg_ir, arg_op = gen_expr_ir_internal env arg_expr in
          let spill_slot = alloc_temp_stack_slot env in
          let spill_ir = [Store (arg_op, spill_slot)] in
          (acc_ir @ arg_ir @ spill_ir, spill_slot :: acc_locs)
        ) ([], []) args
      in
      let arg_locs = List.rev arg_spill_locs_rev in

      (* 步骤 2: 区分需要通过寄存器和栈传递的参数 *)
      let reg_arg_locs, stack_arg_locs =
        let rec split n lst =
            if n <= 0 then ([], lst)
            else
            match lst with
            | [] -> ([], [])
            | h :: t ->
                let (taken, rest) = split (n - 1) t in
                (h :: taken, rest)
          in
          split 8 arg_locs
      in
      let num_stack_args = List.length stack_arg_locs in
      let stack_space_for_args = num_stack_args * 4 in

      (* 步骤 3: 按照 ABI 顺序生成 IR *)
      (* 3.1: (PreCall) 先为出参分配栈空间 *)
      let pre_call_ir = [PreCall stack_space_for_args] in

      (* 3.2: (Store) 将需要通过栈传递的参数，从其临时位置加载并存入刚分配的出参栈空间 (sp-relative) *)
      let stack_passing_ir =
        List.concat (
          List.mapi (fun i loc ->
            (* 使用 t6 作为中转寄存器 *)
            [ Load (Reg "t6", loc);
              StoreOutArg (Reg "t6", i * 4) ] (* MODIFIED: Use new IR node *)
          ) stack_arg_locs
        )
      in

      (* 3.3: (Load) 将需要通过寄存器传递的参数，从其临时位置加载到 a0-a7 *)
      let reg_passing_ir =
        List.mapi (fun i loc ->
          Load (Reg ("a" ^ string_of_int i), loc)
        ) reg_arg_locs
      in

      (* 3.4: (Call) 生成真正的调用指令，以及后续清理和返回值处理 *)
      let temp_ret_op = fresh_temp env in
      let call_cleanup_ir = [
        Call fname;
        PostCall stack_space_for_args;
        Move (temp_ret_op, Reg "a0")
      ] in

      (* 最终的 IR 顺序:
         求值 -> 准备调用栈 -> 传递栈参数 -> 传递寄存器参数 -> 调用和清理 *)
      let full_ir = eval_ir @ pre_call_ir @ stack_passing_ir @ reg_passing_ir @ call_cleanup_ir in
      (full_ir, temp_ret_op)
  
(* _ -> failwith "Unsupported expression type in codegen" *)


let rec gen_stmt_ir_internal (env: cg_env) ?break_lbl ?cont_lbl (s: stmt) : ir list * cg_env =
  env.temp_counter := 0; (* MODIFIED *)

  match s with
  | Expr e ->
      let (ir, _) = gen_expr_ir_internal env e in
      (ir, env) (* The environment does not change for an expression statement. *)

  | Return None -> ([Ret], env)
  | Return (Some e) ->
      let (ir, op) = gen_expr_ir_internal env e in
      (ir @ [Move (Reg "a0", op); Ret], env)

  | VarDecl (id, init_e) ->
      (* 1. Generate IR for the initializer using the CURRENT environment. *)
      let (init_ir, op) = gen_expr_ir_internal env init_e in

      (* 2. Allocate stack space for the new variable. *)
      env.stack_top := !(env.stack_top) - 4;
      let var_loc = !(env.stack_top) in

      (* 3. Create the store instruction. *)
      let store_ir = [Store (op, Stack var_loc)] in

      (* 4. Create the NEW environment for subsequent statements by updating the vars list. *)
      let new_current_scope = VarEnv.add id var_loc (List.hd env.vars) in
      let new_env = { env with vars = new_current_scope :: (List.tl env.vars) } in

      (init_ir @ store_ir, new_env) (* Return the IR and the MODIFIED environment. *)

  | Block stmts ->
      (* 1. Enter a new scope by pushing an empty var map. *)
      let block_env = { env with vars = VarEnv.empty :: env.vars } in
      (* 2. Process the statements within the new scope. *)
      let (block_ir, _) = gen_stmts_ir_internal block_env ?break_lbl ?cont_lbl stmts in
      (* 3. Exit the scope by returning the original environment. *)
      (block_ir, env)

  | If (cond, then_s, else_s_opt) ->
      (* All sub-expressions and statements will now use the same `env` instance,
         ensuring the label_counter is correctly incremented and never reset. *)
      let (cond_ir, cond_op) = gen_expr_ir_internal env cond in
      let else_label = fresh_label env "L_else_" in
      let end_label = fresh_label env "L_end_" in
      let (then_ir, _) = gen_stmt_ir_internal env ?break_lbl ?cont_lbl then_s in
      (match else_s_opt with
      | None ->
          (cond_ir @ [BranchZ (cond_op, end_label)] @ then_ir @ [Label end_label], env)
      | Some else_s ->
          let (else_ir, _) = gen_stmt_ir_internal env ?break_lbl ?cont_lbl else_s in
          (cond_ir @ [BranchZ (cond_op, else_label)] @ then_ir @ [Jump end_label; Label else_label] @ else_ir @ [Label end_label], env)
      )
  | While (cond, body) ->
      let start_label = fresh_label env "L_while_start_" in
      let end_label = fresh_label env "L_while_end_" in
      let (cond_ir, cond_op) = gen_expr_ir_internal env cond in
      let (body_ir, _) = gen_stmt_ir_internal env ~break_lbl:end_label ~cont_lbl:start_label body in
      ([Label start_label] @ cond_ir @ [BranchZ (cond_op, end_label)] @ body_ir @ [Jump start_label; Label end_label], env)

  | Break -> (match break_lbl with Some lbl -> ([Jump lbl], env) | None -> failwith "break statement not within a loop")
  | Continue -> (match cont_lbl with Some lbl -> ([Jump lbl], env) | None -> failwith "continue statement not within a loop")

(* This helper function processes a LIST of statements,
 * passing the updated environment from one statement to the next. *)
and gen_stmts_ir_internal (env: cg_env) ?break_lbl ?cont_lbl (stmts: stmt list) : ir list * cg_env =
  List.fold_left
    (fun (acc_ir, current_env) stmt ->
      let (new_ir, next_env) = gen_stmt_ir_internal current_env ?break_lbl ?cont_lbl stmt in
      (acc_ir @ new_ir, next_env)
    )
    ( [], env )
    stmts

(* MODIFIED: This function now correctly calculates stack size after generating the body IR. *)
let gen_func_ir_internal (ana: analysis_result) (f: func_def) : ir list =
  (* 1. Setup initial environment for parameters *)
  let param_offset = ref (-8) in
  let params_with_offsets =
    List.mapi (fun i name ->
      param_offset := !param_offset - 4;
      let offset = if i < 8 then !param_offset else 8 + (i - 8) * 4 in
      let reg_opt = if i < 8 then Some (Reg ("a" ^ string_of_int i)) else None in
      (name, offset, reg_opt)
    ) f.params
  in
  let initial_var_map = List.fold_left (fun acc (name, offset, _) -> VarEnv.add name offset acc) VarEnv.empty params_with_offsets in

  (* 2. Create the initial generation environment *)
  let env = {
  funcs = ana.global_funcs;
    vars = [initial_var_map]; (* Start with one scope for parameters *)
    stack_top = ref !param_offset;
    temp_counter = ref 0;        (* MODIFIED *)
    label_counter = ref 0;       (* MODIFIED *)
    current_function_name = f.fname; (* INITIALIZE HERE *)
} in

  (* 3. Generate the IR for the function body using the new helper. *)
  let (body_ir, final_env) = gen_stmts_ir_internal env f.body in

  (* 4. Save parameters from registers to stack *)
  let params_save_ir =
    List.filter_map (function (_, offset, Some reg) -> Some (Store (reg, Stack offset)) | _ -> None) params_with_offsets
  in

  (* 5. Calculate final stack size using the final environment's stack_top. *)
  let required_stack = abs !(final_env.stack_top) + 8 in
  let stack_size = if required_stack mod 16 == 0 then required_stack else required_stack + (16 - required_stack mod 16) in

  (* 6. Assemble the full function IR *)
  [Prologue (f.fname, stack_size)] @ params_save_ir @ body_ir @ [Epilogue (f.fname, stack_size)]


(*******************************************************************
 * 3. 从 IR 到 RISC-V 汇编的转换 (内部函数)
 *******************************************************************)

(* In Section 3 - FINAL, CORRECTED VERSION of ir_to_asm_list_internal *)

let ir_to_asm_list_internal (ir_instr: ir) : string list =
  let is_small_imm i = i >= -2048 && i <= 2047 in
  let emit_mem_access op_str reg_name offset base_reg =
    if is_small_imm offset then
      [Printf.sprintf "  %s %s, %d(%s)" op_str reg_name offset base_reg]
    else
      [Printf.sprintf "  li t6, %d" offset;
       Printf.sprintf "  add t6, %s, t6" base_reg;
       Printf.sprintf "  %s %s, 0(t6)" op_str reg_name]
  in
  let ensure_in_reg op target_reg =
    match op with
    | Reg s ->
        if s = target_reg then [], s else [Printf.sprintf "  mv %s, %s" target_reg s], target_reg
    | Stack i ->
        emit_mem_access "lw" target_reg i "fp", target_reg
    | Imm i ->
        [Printf.sprintf "  li %s, %d" target_reg i], target_reg
  in
  let store_from_reg src_reg dest_op =
    match dest_op with
    | Reg s ->
        if s = src_reg then [] else [Printf.sprintf "  mv %s, %s" s src_reg]
    | Stack i ->
        emit_mem_access "sw" src_reg i "fp"
    | Imm _ -> failwith "FATAL: Cannot store into an immediate value"
  in

  match ir_instr with
  | Label s -> [s ^ ":"]
  | Li (dest, imm) ->
      let load_imm_ir = [Printf.sprintf "  li t6, %d" imm] in
      let store_ir = store_from_reg "t6" dest in
      load_imm_ir @ store_ir
  | Move (dest, src) ->
      let load_ir, src_reg_name = ensure_in_reg src "t6" in
      let store_ir = store_from_reg src_reg_name dest in
      load_ir @ store_ir
  | Load (dest, src) ->
      (* RESTORED: Load is only for fp-relative access *)
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
      let op_str = match op with IR_Neg -> "neg" | IR_Not -> "seqz" in
      let load_ir, src_reg = ensure_in_reg src "t6" in
      let compute_ir = [Printf.sprintf "  %s t6, %s" op_str src_reg] in
      let store_ir = store_from_reg "t6" dest in
      load_ir @ compute_ir @ store_ir
      
  (*** NEW, ROBUST BinOp ASSEMBLY LOGIC ***)
  | BinOp (op, dest, src1, src2) ->
      let op_str = match op with | IR_Add -> "add" | IR_Sub -> "sub" | IR_Mul -> "mul" | IR_Div -> "div" | IR_Mod -> "rem" | _ -> failwith "Invalid op for BinOp" in
      
      (* Step 1: Safely load src1 and src2 into t5 and t6, avoiding clobbering. *)
      let load_ir, r1, r2 =
        match src1, src2 with
        (* If both are already registers, just use them. *)
        | Reg r1, Reg r2 -> [], r1, r2
        (* If one is a register and the other is not, load the non-reg one. *)
        | Reg r1, other ->
            let load2_ir, r2 = ensure_in_reg other "t6" in
            load2_ir, r1, r2
        | other, Reg r2 ->
            let load1_ir, r1 = ensure_in_reg other "t5" in
            load1_ir, r1, r2
        (* If neither are registers, load both. *)
        | other1, other2 ->
            let load1_ir, r1 = ensure_in_reg other1 "t5" in
            let load2_ir, r2 = ensure_in_reg other2 "t6" in
            load1_ir @ load2_ir, r1, r2
      in
      
      (* Step 2: Perform the computation, placing result in t5. *)
      let compute_ir = [Printf.sprintf "  %s t5, %s, %s" op_str r1 r2] in
      
      (* Step 3: Store the result from t5 to the final destination. *)
      let store_ir = store_from_reg "t5" dest in
      
      load_ir @ compute_ir @ store_ir
  | BranchZ (src, label) ->
      let load_ir, reg = ensure_in_reg src "t6" in
      load_ir @ [Printf.sprintf "  beqz %s, %s" reg label]
  | BranchNZ (src, label) ->
      let load_ir, reg = ensure_in_reg src "t6" in
      load_ir @ [Printf.sprintf "  bnez %s, %s" reg label]    
      
      
  | Branch (op, src1, src2, label) ->
      (* This logic can also be made safer, similar to BinOp *)
      let branch_op_str = match op with | IR_Eq -> "beq" | IR_Neq -> "bne" | IR_Lt -> "blt" | IR_Le -> "ble" | IR_Gt -> "bgt" | IR_Ge -> "bge" | _ -> failwith "Invalid op for Branch" in
      let load_ir, r1, r2 =
        match src1, src2 with
        | Reg r1, Reg r2 -> [], r1, r2
        | Reg r1, other -> let load2_ir, r2 = ensure_in_reg other "t6" in load2_ir, r1, r2
        | other, Reg r2 -> let load1_ir, r1 = ensure_in_reg other "t5" in load1_ir, r1, r2
        | other1, other2 ->
            let load1_ir, r1 = ensure_in_reg other1 "t5" in
            let load2_ir, r2 = ensure_in_reg other2 "t6" in
            load1_ir @ load2_ir, r1, r2
      in
      let branch_ir = [Printf.sprintf "  %s %s, %s, %s" branch_op_str r1 r2 label] in
      load_ir @ branch_ir

  (* ... All other cases from PreCall to Epilogue remain the same as the last version ... *)
  | Jump s -> [Printf.sprintf "  j %s" s]
  | Ret -> failwith "Ret should not be directly converted, it's handled by Epilogue"
  | PreCall (stack_space) ->
      if stack_space > 0 then
        (if is_small_imm (-stack_space) then [Printf.sprintf "  addi sp, sp, -%d" stack_space]
         else [Printf.sprintf "  li t6, %d" stack_space; Printf.sprintf "  sub sp, sp, t6"])
      else []
  | Call s ->
      [Printf.sprintf "  call %s" s]
  | PostCall (stack_space) ->
      if stack_space > 0 then
        (if is_small_imm stack_space then [Printf.sprintf "  addi sp, sp, %d" stack_space]
         else [Printf.sprintf "  li t6, %d" stack_space; Printf.sprintf "  add sp, sp, t6"])
      else []
  | Prologue (fname, stack_size) ->
      let setup_sp =
        if is_small_imm (-stack_size) then
          [Printf.sprintf "  addi sp, sp, -%d" stack_size]
        else
          [Printf.sprintf "  li t6, %d" stack_size;
           Printf.sprintf "  sub sp, sp, t6"]
      in
      let save_ra = emit_mem_access "sw" "ra" (stack_size - 4) "sp" in
      let save_fp = emit_mem_access "sw" "fp" (stack_size - 8) "sp" in
      let setup_fp =
        if is_small_imm stack_size then
          [Printf.sprintf "  addi fp, sp, %d" stack_size]
        else
          [Printf.sprintf "  li t6, %d" stack_size;
           Printf.sprintf "  add fp, sp, t6"]
      in
      [".text"; ".globl " ^ fname; fname ^ ":"] @ setup_sp @ save_ra @ save_fp @ setup_fp
  | Epilogue (fname, stack_size) ->
      let restore_fp = emit_mem_access "lw" "fp" (stack_size - 8) "sp" in
      let restore_ra = emit_mem_access "lw" "ra" (stack_size - 4) "sp" in
      let teardown_sp =
        if is_small_imm stack_size then
          [Printf.sprintf "  addi sp, sp, %d" stack_size]
        else
          [Printf.sprintf "  li t6, %d" stack_size;
           Printf.sprintf "  add sp, sp, t6"]
      in
      [".L_ret_" ^ fname ^ ":"] @ restore_fp @ restore_ra @ teardown_sp @ ["  ret"]

(*******************************************************************
 * 4. 公共接口 (Public Interface)
 *******************************************************************)
(* This section remains unchanged *)
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
  | StoreOutArg (src, offset) -> Printf.sprintf "  *(sp + %d) = %s" offset (op_to_str src)
  | Jump s -> Printf.sprintf "  j %s" s
  | PreCall size -> Printf.sprintf "  precall %d" size
  | Call s -> Printf.sprintf "  call %s" s
  | PostCall size -> Printf.sprintf "  postcall %d" size
  | Ret -> "  ret"
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
  generate_code ast