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

(* In codegen.ml, replace the original cg_env *)
type cg_env = {
  funcs: func_sig FuncEnv.t;
  vars: (int VarEnv.t) list ref; (* 作用域栈: list的头部是当前作用域 *)
  mutable stack_top: int;
  mutable temp_counter: int;
  mutable label_counter: int;
}

(* In codegen.ml, add these new helper functions *)

(* 在作用域栈中查找变量，正确处理遮蔽 (shadowing) *)
let find_var_loc var_name env =
  let rec find_in_scopes scopes =
    match scopes with
    | [] -> failwith ("Undeclared variable or variable used before declaration: " ^ var_name)
    | current_scope :: outer_scopes ->
        (match VarEnv.find_opt var_name current_scope with
        | Some loc -> loc
        | None -> find_in_scopes outer_scopes)
  in
  find_in_scopes !(env.vars)

(* 将一个新变量及其栈位置添加到当前作用域 (栈顶) *)
let add_var_to_current_scope var_name loc env =
  match !(env.vars) with
  | [] -> failwith "Cannot add variable: scope stack is empty."
  | current_scope :: outer_scopes ->
      if VarEnv.mem var_name current_scope then
        failwith ("Variable '" ^ var_name ^ "' is already declared in this scope.");
      let new_current_scope = VarEnv.add var_name loc current_scope in
      env.vars := new_current_scope :: outer_scopes

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

  (* In codegen.ml, replace the existing versions of these three functions *)

let rec gen_expr_ir_internal env (e: expr) : ir list * operand =
  match e with
  | IntLiteral n ->
      let temp_reg = fresh_temp_reg env in
      [Li (temp_reg, n)], temp_reg
  | Id x ->
      let var_loc = find_var_loc x env in (* MODIFIED: Use new lookup function *)
      let temp_reg = fresh_temp_reg env in
      [Load (temp_reg, Stack var_loc)], temp_reg
  | Assign (x, rhs_expr) ->
      let rhs_ir, rhs_op = gen_expr_ir_internal env rhs_expr in
      let var_loc = find_var_loc x env in (* MODIFIED: Use new lookup function *)
      rhs_ir @ [Store (rhs_op, Stack var_loc)], rhs_op
  | UnOp (op, expr) ->
      let expr_ir, expr_op = gen_expr_ir_internal env expr in
      let dest_reg = fresh_temp_reg env in
      expr_ir @ [UnOp (unop_from_ast_op op, dest_reg, expr_op)], dest_reg
  | BinOp (op, e1, e2) ->
      (* This part of the function remains the same as your original code *)
      (match op with
      | And ->
          let dest_reg = fresh_temp_reg env in
          let false_label = fresh_label env "L_false_" in
          let end_label = fresh_label env "L_end_" in
          let ir1, op1 = gen_expr_ir_internal env e1 in
          env.temp_counter <- 0;
          let ir2, op2 = gen_expr_ir_internal env e2 in
          ir1 @ [BranchZ(op1, false_label)] @ ir2 @ [BranchZ(op2, false_label)] @
          [Li(dest_reg, 1); Jump(end_label); Label(false_label); Li(dest_reg, 0); Label(end_label)], dest_reg
      | Or ->
          let dest_reg = fresh_temp_reg env in
          let true_label = fresh_label env "L_true_" in
          let end_label = fresh_label env "L_end_" in
          let ir1, op1 = gen_expr_ir_internal env e1 in
          env.temp_counter <- 0;
          let ir2, op2 = gen_expr_ir_internal env e2 in
          ir1 @ [BranchNZ(op1, true_label)] @ ir2 @ [BranchNZ(op2, true_label)] @
          [Li(dest_reg, 0); Jump(end_label); Label(true_label); Li(dest_reg, 1); Label(end_label)], dest_reg
      | _ ->
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
      (* This part of the function remains the same as your original code *)
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

and gen_stmt_ir_internal env ?break_lbl ?cont_lbl (s: stmt) : ir list =
  env.temp_counter <- 0;
  match s with
  | Expr e -> fst (gen_expr_ir_internal env e)
  | Return None -> [Ret]
  | Return (Some e) ->
      let ir, op = gen_expr_ir_internal env e in
      ir @ [Move (Reg "a0", op); Ret]
  | VarDecl (id, init_e) ->
      (* MODIFIED: Allocate space and add to *current* scope *)
      let ir, op = gen_expr_ir_internal env init_e in
      let var_loc = alloc_temp_stack_slot env in (* Use the stack allocator *)
      add_var_to_current_scope id (match var_loc with Stack i -> i | _ -> failwith "impossible") env;
      ir @ [Store (op, var_loc)]
  | Block stmts ->
      (* MODIFIED: Manage scope stack *)
      env.vars := VarEnv.empty :: !(env.vars); (* Enter scope: push an empty map *)
      let block_ir = List.concat_map (gen_stmt_ir_internal env ?break_lbl ?cont_lbl) stmts in
      env.vars := List.tl !(env.vars); (* Exit scope: pop the map *)
      block_ir
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

let gen_func_ir_internal (ana: Semantic.analysis_result) (f: func_def) : ir list =
  (* MODIFIED: Pre-calculate stack size correctly *)
  let num_local_decls = List.assoc f.fname ana.local_var_counts in
  let num_temps = 6 in (* Max temp registers t0-t5 *)
  (* Total space needed: return address, old frame pointer, params, all local decls, and spill space for all temps *)
  let required_stack = 8 + (List.length f.params * 4) + (num_local_decls * 4) + (num_temps * 4) in
  (* Align to 16-byte boundary *)
  let stack_size = if required_stack mod 16 == 0 then required_stack else required_stack + (16 - required_stack mod 16) in

  (* MODIFIED: Setup environment with a scope stack for parameters *)
  let param_offset = ref (-8) in
  let params_with_offsets =
    List.map (fun name ->
      let offset = !param_offset in
      param_offset := !param_offset - 4;
      (name, offset, Reg ("a" ^ string_of_int (List.length f.params - (offset + 8) / -4 -1)))
    ) (List.rev f.params) (* Reverse to assign a0 to first param etc. *)
  in
  let param_map = List.fold_left (fun acc (name, offset, _) -> VarEnv.add name offset acc) VarEnv.empty params_with_offsets in
  
  let env = {
    funcs = ana.global_funcs;
    vars = ref [param_map]; (* Initialize scope stack with a map for parameters *)
    stack_top = !param_offset; (* Local variables will be allocated after parameters *)
    temp_counter = 0;
    label_counter = 0;
  } in

  (* Create IR to save parameters (a0-a7) to their assigned stack slots *)
  let params_save_ir =
    List.map (fun (_, offset, reg) -> Store (reg, Stack offset)) (List.filteri (fun i _ -> i < 8) params_with_offsets)
  in

  let body_ir = List.concat_map (gen_stmt_ir_internal env) f.body in

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
  |
Imm of int          (* 立即数, e.g., 5 *)
  |
Reg of string       (* 物理或虚拟寄存器, e.g., "t0", "a0" *)
  |
Stack of int        (* 栈上位置 (相对于fp的偏移量), e.g., -4, -8 *)

(* * 为了在没有 .mli 文件的情况下打破模块间的类型依赖，
 * 我们在内部复制一份操作符的定义。
 *)
type ir_binop = |
IR_Add | IR_Sub | IR_Mul | IR_Div | IR_Mod
                |
IR_Eq | IR_Neq | IR_Lt | IR_Le | IR_Gt |
IR_Ge
                | IR_And |
IR_Or
type ir_unop = IR_Neg | IR_Not

(**
 * 中间表示 (IR) 类型。
 * 这是 codegen.mli 中 `type ir` 的具体实现。
 *)
type ir =
  |
Label of string
  | Li of operand * int
  | Move of operand * operand
  |
BinOp of ir_binop * operand * operand * operand
  | UnOp of ir_unop * operand * operand
  |
Load of operand * operand
  | Store of operand * operand
  |
Branch of ir_binop * operand * operand * string
  | BranchZ of operand * string
  |
BranchNZ of operand * string
  | Jump of string
  | Call of string * int
  |
Ret
  | Prologue of string * int
  |
Epilogue of string * int

(* 代码生成环境 (内部使用) *)
type cg_env = {
  funcs: func_sig FuncEnv.t;
  vars: int VarEnv.t list; (* A stack of scopes *)
  stack_top: int ref;      (* Mutable stack offset counter *)
  mutable temp_counter: int;
  mutable label_counter: int;
  current_function_name: string; (* NEW: Store the current function's name *)
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

(* 创建一个新的临时寄存器名 (t0-t5) *)
let fresh_temp_reg env =
  if env.temp_counter >= 6 then failwith "Expression too complex, ran out of temporary registers";
let reg_name = "t" ^ string_of_int env.temp_counter in
  env.temp_counter <- env.temp_counter + 1;
Reg reg_name

(* 创建一个新的标签 *)
(* let fresh_label env pfx =
  let label_name = pfx ^ string_of_int env.label_counter in
  env.label_counter <- env.label_counter + 1;
label_name *)
let fresh_label env pfx =
  let label_name = Printf.sprintf "%s_%s%d" env.current_function_name pfx env.label_counter in
  env.label_counter <- env.label_counter + 1;
  label_name

(* 在栈上为临时计算结果分配空间 *)
let alloc_temp_stack_slot env =
  env.stack_top := !(env.stack_top) - 4;
Stack !(env.stack_top)

(* 将 Ast 操作符转换为内部 IR 操作符的辅助函数 *)
let binop_from_ast_op op =
  match op with
  |
Add -> IR_Add | Sub -> IR_Sub | Mul -> IR_Mul | Div -> IR_Div |
Mod -> IR_Mod
  | Eq -> IR_Eq | Neq -> IR_Neq | Lt -> IR_Lt |
Le -> IR_Le | Gt -> IR_Gt | Ge -> IR_Ge
  | And -> IR_And |
Or -> IR_Or

let unop_from_ast_op op =
  match op with
  | Neg -> IR_Neg |
Not -> IR_Not

(*******************************************************************
 * 2. 从 AST 到 IR 的转换 (内部函数)
 *******************************************************************)

let rec gen_expr_ir_internal env (e: expr) : ir list * operand =
  match e with
  |
IntLiteral n ->
      let temp_reg = fresh_temp_reg env in
      [Li (temp_reg, n)], temp_reg
  |
Id x ->
      let var_loc = find_var_offset env x in
      let temp_reg = fresh_temp_reg env in
      [Load (temp_reg, Stack var_loc)], temp_reg
  |
Assign (x, rhs_expr) ->
      let rhs_ir, rhs_op = gen_expr_ir_internal env rhs_expr in
      let var_loc = find_var_offset env x in
      rhs_ir @ [Store (rhs_op, Stack var_loc)], rhs_op
  |
UnOp (op, expr) ->
      let expr_ir, expr_op = gen_expr_ir_internal env expr in
      let dest_reg = fresh_temp_reg env in
      expr_ir @ [UnOp (unop_from_ast_op op, dest_reg, expr_op)], dest_reg
  |
BinOp (op, e1, e2) ->
      (match op with
      | And ->
          let dest_reg = fresh_temp_reg env in
          let false_label = fresh_label env "L_false_" in
          let end_label = fresh_label env "L_end_" in
          let ir1, op1 = gen_expr_ir_internal env e1 in
          env.temp_counter <- 0; (* Reset for 
e2 *)
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
          [Li(dest_reg, 0);
Jump(end_label); Label(true_label); Li(dest_reg, 1); Label(end_label)], dest_reg
      |
_ ->
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
          | IR_Eq | IR_Neq | IR_Lt | IR_Le | IR_Gt | 
IR_Ge ->
              let true_label = fresh_label env "L_true_" in
              let end_label = fresh_label env "L_end_" in
              ir2 @ save_ir @ ir1 @ load_ir @
              [Li(dest_reg, 0); Branch(final_op, op1, loaded_op2, true_label); Jump(end_label);
               Label(true_label); Li(dest_reg, 1); 
Label(end_label)], dest_reg
          | _ ->
              ir2 @ save_ir @ ir1 @ load_ir @ [BinOp(final_op, dest_reg, op1, loaded_op2)], dest_reg
          )
      )
  |
Call (fname, args) ->
      let args_code_and_ops = List.map (gen_expr_ir_internal env) args in
      let args_code = List.concat_map fst args_code_and_ops in
      let arg_ops = List.map snd args_code_and_ops in
      let reg_args, stack_args =
        let rec split n lst = if n <= 0 then ([], lst) else match lst with |
[] -> ([], []) | h :: t -> let (taken, rest) = split (n - 1) t in (h :: taken, rest)
        in split 8 arg_ops
      in
      let reg_passing_ir = List.mapi (fun i op -> Move (Reg ("a" ^ string_of_int i), op)) reg_args in
      let stack_passing_ir = List.mapi (fun i op -> Store (op, Stack (i * -4))) (List.rev stack_args) in
      let num_stack_args = List.length stack_args in
      
let ret_reg = Reg "a0" in
      let temp_ret_reg = fresh_temp_reg env in
      let call_ir = [Call (fname, num_stack_args);
Move (temp_ret_reg, ret_reg)] in
      args_code @ stack_passing_ir @ reg_passing_ir @ call_ir, temp_ret_reg
(* _ -> failwith "Unsupported expression type in codegen" *)

(*******************************************************************
 * MODIFIED: Statement generation logic
 * REASON: To correctly handle variable declarations, the functions
 * must now thread the environment through the statement processing.
 * They now return `(ir list * cg_env)` instead of just `ir list`.
 * The functions are now correctly defined as a mutually recursive group.
 *******************************************************************)

(* `gen_stmt_ir_internal` and `gen_stmts_ir_internal` are mutually recursive,
 * so they must be defined together in a single `let rec ... and ...` block. *)
let rec gen_stmt_ir_internal (env: cg_env) ?break_lbl ?cont_lbl (s: stmt) : ir list * cg_env =
  let temp_reset_env = { env with temp_counter = 0 } in
  match s with
  | Expr e ->
      let (ir, _) = gen_expr_ir_internal temp_reset_env e in
      (ir, env) (* The environment does not change for an expression statement. *)

  | Return None -> ([Ret], env)
  | Return (Some e) ->
      let (ir, op) = gen_expr_ir_internal temp_reset_env e in
      (ir @ [Move (Reg "a0", op); Ret], env)

  | VarDecl (id, init_e) ->
      (* 1. Generate IR for the initializer using the CURRENT environment. *)
      let (init_ir, op) = gen_expr_ir_internal temp_reset_env init_e in

      (* 2. Allocate stack space for the new variable. *)
      temp_reset_env.stack_top := !(temp_reset_env.stack_top) - 4;
      let var_loc = !(temp_reset_env.stack_top) in

      (* 3. Create the store instruction. *)
      let store_ir = [Store (op, Stack var_loc)] in

      (* 4. Create the NEW environment for subsequent statements. *)
      let new_current_scope = VarEnv.add id var_loc (List.hd temp_reset_env.vars) in
      let new_env = { temp_reset_env with vars = new_current_scope :: (List.tl temp_reset_env.vars) } in

      (init_ir @ store_ir, new_env) (* Return the IR and the MODIFIED environment. *)

  | Block stmts ->
      (* 1. Enter a new scope by pushing an empty var map. *)
      let block_env = { temp_reset_env with vars = VarEnv.empty :: temp_reset_env.vars } in
      (* 2. Process the statements within the new scope. *)
      let (block_ir, _) = gen_stmts_ir_internal block_env ?break_lbl ?cont_lbl stmts in
      (* 3. Exit the scope by returning the original environment. *)
      (block_ir, env)

  | If (cond, then_s, else_s_opt) ->
      let (cond_ir, cond_op) = gen_expr_ir_internal temp_reset_env cond in
      let else_label = fresh_label temp_reset_env "L_else_" in
      let end_label = fresh_label temp_reset_env "L_end_" in
      (* Scopes within branches do not affect the outer scope. *)
      let (then_ir, _) = gen_stmt_ir_internal temp_reset_env ?break_lbl ?cont_lbl then_s in
      (match else_s_opt with
      | None ->
          (cond_ir @ [BranchZ (cond_op, end_label)] @ then_ir @ [Label end_label], env)
      | Some else_s ->
          let (else_ir, _) = gen_stmt_ir_internal temp_reset_env ?break_lbl ?cont_lbl else_s in
          (cond_ir @ [BranchZ (cond_op, else_label)] @ then_ir @ [Jump end_label; Label else_label] @ else_ir @ [Label end_label], env)
      )
  | While (cond, body) ->
      let start_label = fresh_label temp_reset_env "L_while_start_" in
      let end_label = fresh_label temp_reset_env "L_while_end_" in
      let (cond_ir, cond_op) = gen_expr_ir_internal temp_reset_env cond in
      (* The loop body is a self-contained scope. *)
      let (body_ir, _) = gen_stmt_ir_internal temp_reset_env ~break_lbl:end_label ~cont_lbl:start_label body in
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
    temp_counter = 0;
    label_counter = 0;
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
(* This section remains unchanged *)
let ir_to_asm_list_internal (ir_instr: ir) : string list =
  let op_to_str op = 
match op with
    | Imm i -> string_of_int i
    |
Reg s -> s
    | Stack i -> Printf.sprintf "%d(fp)" i
  in
  match ir_instr with
  |
Label s -> [s ^ ":"]
  | Li (dest, imm) -> [Printf.sprintf "  li %s, %d" (op_to_str dest) imm]
  |
Move (dest, src) -> [Printf.sprintf "  mv %s, %s" (op_to_str dest) (op_to_str src)]
  |
Load (dest, src) -> [Printf.sprintf "  lw %s, %s" (op_to_str dest) (op_to_str src)]
  |
Store (src, Stack i) when i < 0 -> [Printf.sprintf "  sw %s, %d(fp)" (op_to_str src) i]
  |
Store (src, Stack i) -> [Printf.sprintf "  sw %s, %d(sp)" (op_to_str src) i]
  |
Store (src, dest) -> [Printf.sprintf "  sw %s, %s" (op_to_str src) (op_to_str dest)]
  |
Jump s -> [Printf.sprintf "  j %s" s]
  |
Ret -> failwith "Ret should not be directly converted, it's handled by Epilogue"
  |
Call (s, num_stack_args) ->
      let stack_space = num_stack_args * 4 in
      if stack_space > 0 then
        [Printf.sprintf "  addi sp, sp, -%d" stack_space;
Printf.sprintf "  call %s" s;
         Printf.sprintf "  addi sp, sp, %d" stack_space]
      else [Printf.sprintf "  call %s" s]
  |
UnOp (op, dest, src) ->
      let op_str = match op with IR_Neg -> "neg" |
IR_Not -> "seqz" in
      [Printf.sprintf "  %s %s, %s" op_str (op_to_str dest) (op_to_str src)]
  |
BinOp (op, dest, src1, src2) ->
      let op_str = match op with |
IR_Add -> "add" | IR_Sub -> "sub" | IR_Mul -> "mul" | IR_Div -> "div" | IR_Mod -> "rem" |
_ -> failwith "Invalid op" in
      [Printf.sprintf "  %s %s, %s, %s" op_str (op_to_str dest) (op_to_str src1) (op_to_str src2)]
  |
BranchZ (src, label) -> [Printf.sprintf "  beqz %s, %s" (op_to_str src) label]
  |
BranchNZ (src, label) -> [Printf.sprintf "  bnez %s, %s" (op_to_str src) label]
  |
Branch (op, src1, src2, label) ->
      let branch_op_str = match op with |
IR_Eq -> "beq" | IR_Neq -> "bne" | IR_Lt -> "blt" | IR_Le -> "ble" | IR_Gt -> "bgt" |
IR_Ge -> "bge" | _ -> failwith "Invalid op" in
      (match src1, src2 with
       | Reg _, Reg _ -> [Printf.sprintf "  %s %s, %s, %s" branch_op_str (op_to_str src1) (op_to_str src2) label]
       | Reg r, Imm i -> [Printf.sprintf "  li t6, %d" i; Printf.sprintf "  %s %s, t6, %s" branch_op_str r label]
       | Imm i, Reg r -> [Printf.sprintf "  li t6, %d" i; Printf.sprintf "  %s t6, %s, %s" branch_op_str 
r label]
       | _ -> failwith "Invalid operands for branch")
  |
Prologue (fname, stack_size) ->
      [ "  .text"; "  .globl " ^ fname;
fname ^ ":";
        Printf.sprintf "  addi sp, sp, -%d" stack_size;
        Printf.sprintf "  sw ra, %d(sp)" (stack_size - 4);
Printf.sprintf "  sw fp, %d(sp)" (stack_size - 8);
        Printf.sprintf "  addi fp, sp, %d" stack_size; ]
  |
Epilogue (fname, stack_size) ->
      [ ".L_ret_" ^ fname ^ ":";
Printf.sprintf "  lw fp, %d(sp)" (stack_size - 8);
        Printf.sprintf "  lw ra, %d(sp)" (stack_size - 4);
Printf.sprintf "  addi sp, sp, %d" stack_size;
        "  ret";
]

(*******************************************************************
 * 4. 公共接口 (Public Interface)
 *******************************************************************)
(* This section remains unchanged *)
let string_of_ir (ir_instr: ir) : string =
  let op_to_str op = match op with
    |
Imm i -> string_of_int i
    | Reg s -> s
    |
Stack i -> Printf.sprintf "stack[%d]" i
  in
  let binop_to_str op = match op with
    |
IR_Add -> "+" | IR_Sub -> "-" | IR_Mul -> "*" | IR_Div -> "/" |
IR_Mod -> "%"
    | IR_Eq -> "==" | IR_Neq -> "!=" | IR_Lt -> "<" |
IR_Le -> "<=" | IR_Gt -> ">" | IR_Ge -> ">="
    | IR_And -> "&&" |
IR_Or -> "||"
  in
  match ir_instr with
  | Label s -> s ^ ":"
  |
Li (dest, imm) -> Printf.sprintf "  %s = %d" (op_to_str dest) imm
  |
Move (dest, src) -> Printf.sprintf "  %s = %s" (op_to_str dest) (op_to_str src)
  |
Load (dest, src) -> Printf.sprintf "  %s = *%s" (op_to_str dest) (op_to_str src)
  |
Store (src, dest) -> Printf.sprintf "  *%s = %s" (op_to_str dest) (op_to_str src)
  |
Jump s -> Printf.sprintf "  j %s" s
  | Ret -> "  ret"
  |
Call (s, n) -> Printf.sprintf "  call %s, %d" s n
  |
UnOp (op, dest, src) ->
      let op_str = match op with IR_Neg -> "-" |
IR_Not -> "!" in
      Printf.sprintf "  %s = %s%s" (op_to_str dest) op_str (op_to_str src)
  |
BinOp (op, dest, src1, src2) ->
      Printf.sprintf "  %s = %s %s %s" (op_to_str dest) (op_to_str src1) (binop_to_str op) (op_to_str src2)
  |
BranchZ (src, label) -> Printf.sprintf "  ifz %s j %s" (op_to_str src) label
  |
BranchNZ (src, label) -> Printf.sprintf "  ifnz %s j %s" (op_to_str src) label
  |
Branch (op, src1, src2, label) ->
      Printf.sprintf "  if %s %s %s j %s" (op_to_str src1) (binop_to_str op) (op_to_str src2) label
  |
Prologue (fname, size) -> Printf.sprintf "prologue %s, %d" fname size
  |
Epilogue (fname, size) -> Printf.sprintf "epilogue %s, %d" fname size

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
