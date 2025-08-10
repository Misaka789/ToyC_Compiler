open Ast
open Codegen

(* 辅助函数，用于执行实际的计算 *)
let evaluate_binop op n1 n2 =
  match op with
  | Add -> IntLiteral (n1 + n2)
  | Sub -> IntLiteral (n1 - n2)
  | Mul -> IntLiteral (n1 * n2)
  (* 安全地处理除零错误 *)
  | Div ->
    if n2 <> 0 then IntLiteral (n1 / n2) else raise (Invalid_argument "Division by zero")
  | Mod ->
    if n2 <> 0 then IntLiteral (n1 mod n2) else raise (Invalid_argument "Modulo by zero")
  (* 返回1代表true, 0代表false，这是C语言风格 *)
  | Eq -> IntLiteral (if n1 = n2 then 1 else 0)
  | Neq -> IntLiteral (if n1 <> n2 then 1 else 0)
  | Lt -> IntLiteral (if n1 < n2 then 1 else 0)
  | Le -> IntLiteral (if n1 <= n2 then 1 else 0)
  | Gt -> IntLiteral (if n1 > n2 then 1 else 0)
  | Ge -> IntLiteral (if n1 >= n2 then 1 else 0)
  | And -> IntLiteral (if n1 <> 0 && n2 <> 0 then 1 else 0)
  | Or -> IntLiteral (if n1 <> 0 || n2 <> 0 then 1 else 0)
;;

let evaluate_unop op n =
  match op with
  | Neg -> IntLiteral (-n)
  | Not -> IntLiteral (if n = 0 then 1 else 0)
;;

(* 核心函数：递归地优化一个表达式 *)
let rec optimize_expr (e : expr) : expr =
  match e with
  (* 1. 递归优化子表达式 *)
  | BinOp (op, e1, e2) ->
    let opt_e1 = optimize_expr e1 in
    let opt_e2 = optimize_expr e2 in
    (* 2. 检查优化后的子表达式是否为常量 *)
    (match opt_e1, opt_e2 with
     | IntLiteral n1, IntLiteral n2 ->
       (try evaluate_binop op n1 n2 (* 3. 如果是，进行计算 *) with
        | Invalid_argument _ -> BinOp (op, opt_e1, opt_e2))
       (* 如果计算失败 (如除零)，则不优化 *)
     | _ -> BinOp (op, opt_e1, opt_e2))
    (* 如果不是，保持原样 *)
  | UnOp (op, e1) ->
    let opt_e1 = optimize_expr e1 in
    (match opt_e1 with
     | IntLiteral n1 -> evaluate_unop op n1
     | _ -> UnOp (op, opt_e1))
  (* 对于包含子表达式的结构，递归地调用优化 *)
  | Assign (id, rhs) -> Assign (id, optimize_expr rhs)
  | Call (fname, args) -> Call (fname, List.map optimize_expr args)
  (* 对于没有子表达式或无法优化的表达式，直接返回 *)
  | IntLiteral _ | Id _ -> e
;;

(* 递归地优化一个语句 *)
let rec optimize_stmt (s : stmt) : stmt =
  match s with
  | Expr e -> Expr (optimize_expr e)
  | Return (Some e) -> Return (Some (optimize_expr e))
  | VarDecl (id, init_e) -> VarDecl (id, optimize_expr init_e)
  | If (cond, then_s, else_s_opt) ->
    let opt_cond = optimize_expr cond in
    let opt_then = optimize_stmt then_s in
    let opt_else = Option.map optimize_stmt else_s_opt in
    (* 进阶优化：如果条件是常量，可以直接消除一个分支 *)
    (match opt_cond with
     | IntLiteral n ->
       if n <> 0
       then opt_then
       else (
         match opt_else with
         | Some s -> s
         | None -> Block [])
     | _ -> If (opt_cond, opt_then, opt_else))
  | While (cond, body) ->
    let opt_cond = optimize_expr cond in
    let opt_body = optimize_stmt body in
    (* 进阶优化：如果条件恒为假，可以移除整个循环 *)
    (match opt_cond with
     | IntLiteral 0 -> Block [] (* 循环永不执行 *)
     | _ -> While (opt_cond, opt_body))
  | Block stmts -> Block (List.map optimize_stmt stmts)
  (* 其他无表达式的语句保持不变 *)
  | Return None | Break | Continue -> s
;;

(* 优化一个函数定义 *)
let optimize_func_def (f : func_def) : func_def =
  { f with body = List.map optimize_stmt f.body }
;;

(* 模块的公共接口函数 *)
let optimize_program (p : program) : program = List.map optimize_func_def p

(*实现窥孔优化*)
(* 核心函数：递归地对IR列表进行窥孔优化 *)
(* let rec optimize_ir (ir_list : ir list) : ir list =
  match ir_list with
  (* 模式1: 消除冗余跳转 (跳转到下一条指令) *)
  (*
   * 模式:
   *   Jump "L1"
   *   Label "L1"
   * 优化后:
   *   Label "L1"
   *)
  | Jump label_name :: Label same_label :: tail when label_name = same_label ->
    (* 优化成功！我们丢弃了Jump指令，并对列表的其余部分继续优化 *)
    optimize_ir (Label same_label :: tail)
  (* 模式2: 消除冗余的加载 (在存储后立即加载) *)
  (*
   * 模式:
   *   Store (t0, stack[-12])  ; 将 t0 存入栈
   *   Load (t0, stack[-12])   ; 又从同一个地方加载回 t0
   * 优化后:
   *   Store (t0, stack[-12])
   *)
  | Store (src_op, loc) :: Load (dest_op, same_loc) :: tail
    when loc = same_loc && src_op = dest_op ->
    (* 优化成功！Load指令是多余的，因为值已经在操作数(寄存器)中了。*)
    optimize_ir (Store (src_op, loc) :: tail)
  (* 模式3: 代数化简 (与0或1运算) *)
  (*
   * 模式: dest = src + 0
   * 优化后: dest = src (即 Move 指令)
   *)
  | BinOp (IR_Add, dest, src, Imm 0) :: tail
  | BinOp (IR_Sub, dest, src, Imm 0) :: tail
  | BinOp (IR_Mul, dest, src, Imm 1) :: tail
  | BinOp (IR_Div, dest, src, Imm 1) :: tail -> optimize_ir (Move (dest, src) :: tail)
  (*
     * 模式: dest = src * 0
   * 优化后: dest = 0 (即 Li 指令)
  *)
  | BinOp (IR_Mul, dest, _, Imm 0) :: tail -> optimize_ir (Li (dest, 0) :: tail)
  (* 如果没有模式匹配成功，则保留头部指令，然后继续优化列表的其余部分 *)
  | head :: tail -> head :: optimize_ir tail
  (* 列表为空，递归结束 *)
  | [] -> []
;; *)

(*以上窥孔优化性能没有得到提升 为17.76分*)
let rec optimize_ir_one_pass (ir_list : ir list) : ir list =
  match ir_list with
  (* 模式1: 消除冗余跳转 *)
  | Jump l1 :: Label l2 :: tail when l1 = l2 ->
    (* 优化成功！我们丢弃了Jump指令。
         重点：我们只对列表的 *尾部* (tail) 继续进行单次遍历，而不是从头再来！*)
    optimize_ir_one_pass (Label l2 :: tail)
  (* 模式2: 消除冗余的加载 *)
  | Store (src, loc) :: Load (dest, same_loc) :: tail when loc = same_loc && src = dest ->
    (* 优化成功！丢弃Load指令。继续处理尾部。*)
    Store (src, loc) :: optimize_ir_one_pass tail
  (* 模式3: 代数化简 (加0或乘1) *)
  | BinOp (IR_Add, dest, src, Imm 0) :: tail | BinOp (IR_Sub, dest, src, Imm 0) :: tail ->
    Move (dest, src) :: optimize_ir_one_pass tail
  | BinOp (IR_Mul, dest, src, Imm 1) :: tail | BinOp (IR_Div, dest, src, Imm 1) :: tail ->
    Move (dest, src) :: optimize_ir_one_pass tail
  (* 模式4: 代数化简 (乘0) *)
  | BinOp (IR_Mul, dest, _, Imm 0) :: tail -> Li (dest, 0) :: optimize_ir_one_pass tail
  (* 如果当前指令头部没有匹配任何模式，则保留它，然后继续处理列表的尾部 *)
  | head :: tail -> head :: optimize_ir_one_pass tail
  (* 列表为空，递归结束 *)
  | [] -> []
;;

(* 步骤2: 实现主循环函数，反复调用单次遍历直至代码不再变化 *)
let optimize_ir (ir_code : ir list) : ir list =
  (* 创建一个引用来保存上一次优化的结果 *)
  let prev_ir = ref [] in
  (* 创建一个引用来保存当前优化的结果 *)
  let current_ir = ref ir_code in
  (* 当上一次的结果和当前的结果不同时，就继续循环 *)
  while !prev_ir <> !current_ir do
    prev_ir := !current_ir;
    current_ir := optimize_ir_one_pass !current_ir
  done;
  (* 循环结束，返回最终的优化结果 *)
  !current_ir
;;
(*优化后性能得分 18.19 分*)
