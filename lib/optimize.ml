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

(* 辅助函数 *)
let is_power_of_2 n = n > 0 && n land (n - 1) = 0

let label_is_used label ir_list =
  List.exists
    (function
      | Jump l | BranchZ (_, l) | BranchNZ (_, l) | Branch (_, _, _, l) -> l = label
      | _ -> false)
    ir_list
;;

let rec optimize_ir_enhanced (ir_list : ir list) : ir list =
  match ir_list with
  (* 原有的优化模式 *)
  | Jump l1 :: Label l2 :: tail when l1 = l2 -> optimize_ir_enhanced (Label l2 :: tail)
  (* 新增：消除冗余的move操作 *)
  | Move (dest, src) :: tail when dest = src -> optimize_ir_enhanced tail
  (* 新增：合并连续的立即数加载 *)
  | Li (reg1, n1) :: Li (reg2, n2) :: BinOp (IR_Add, dest, r1, r2) :: tail
    when reg1 = r1 && reg2 = r2 -> Li (dest, n1 + n2) :: optimize_ir_enhanced tail
  (* 新增：消除无用的标签 *)
  | Label l :: tail when not (label_is_used l tail) -> optimize_ir_enhanced tail
  (* 新增：优化连续的栈操作 *)
  | Store (reg, Stack offset1) :: Load (same_reg, Stack offset2) :: tail
    when reg = same_reg && offset1 = offset2 -> optimize_ir_enhanced tail
  | head :: tail -> head :: optimize_ir_enhanced tail
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
    current_ir := optimize_ir_enhanced !current_ir
  done;
  (* 循环结束，返回最终的优化结果 *)
  !current_ir
;;

(* 改进的表达式优化 - 添加更多代数恒等式 *)
let rec optimize_expr_enhanced (e : expr) : expr =
  match e with
  | BinOp (op, e1, e2) ->
    let opt_e1 = optimize_expr_enhanced e1 in
    let opt_e2 = optimize_expr_enhanced e2 in
    (match opt_e1, opt_e2 with
     (* 原有的常量折叠 *)
     | IntLiteral n1, IntLiteral n2 ->
       (try evaluate_binop op n1 n2 with
        | Invalid_argument _ -> BinOp (op, opt_e1, opt_e2))
     (* 新增的代数恒等式 *)
     | IntLiteral 0, e when op = Add -> e
     | e, IntLiteral 0 when op = Add -> e
     | e, IntLiteral 0 when op = Sub -> e
     | IntLiteral 0, _ when op = Mul -> IntLiteral 0
     | _, IntLiteral 0 when op = Mul -> IntLiteral 0
     | IntLiteral 1, e when op = Mul -> e
     | e, IntLiteral 1 when op = Mul -> e
     | e, IntLiteral 1 when op = Div -> e
     | _ -> BinOp (op, opt_e1, opt_e2))
  | UnOp (op, e1) ->
    let opt_e1 = optimize_expr_enhanced e1 in
    (match opt_e1 with
     | IntLiteral n1 -> evaluate_unop op n1
     | _ -> UnOp (op, opt_e1))
  | Assign (id, rhs) -> Assign (id, optimize_expr_enhanced rhs)
  | Call (fname, args) -> Call (fname, List.map optimize_expr_enhanced args)
  | IntLiteral _ | Id _ -> e
;;

(* 改进的语句优化 - 包含死代码消除 *)
let rec optimize_stmt_enhanced (s : stmt) : stmt =
  match s with
  | Expr e -> Expr (optimize_expr_enhanced e)
  | Return (Some e) -> Return (Some (optimize_expr_enhanced e))
  | VarDecl (id, init_e) -> VarDecl (id, optimize_expr_enhanced init_e)
  | If (cond, then_s, else_s_opt) ->
    let opt_cond = optimize_expr_enhanced cond in
    let opt_then = optimize_stmt_enhanced then_s in
    let opt_else = Option.map optimize_stmt_enhanced else_s_opt in
    (* 死代码消除 *)
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
    let opt_cond = optimize_expr_enhanced cond in
    let opt_body = optimize_stmt_enhanced body in
    (* 死代码消除 *)
    (match opt_cond with
     | IntLiteral 0 -> Block []
     | _ -> While (opt_cond, opt_body))
  | Block stmts ->
    let optimized_stmts = List.map optimize_stmt_enhanced stmts in
    (* 过滤空块 *)
    let filtered_stmts =
      List.filter
        (function
          | Block [] -> false
          | _ -> true)
        optimized_stmts
    in
    Block filtered_stmts
  | Return None | Break | Continue -> s
;;

(* 改进的窥孔优化 *)
let rec optimize_ir_enhanced (ir_list : ir list) : ir list =
  match ir_list with
  (* 原有模式 *)
  | Jump l1 :: Label l2 :: tail when l1 = l2 -> optimize_ir_enhanced (Label l2 :: tail)
  | Store (src, loc) :: Load (dest, same_loc) :: tail when loc = same_loc && src = dest ->
    Store (src, loc) :: optimize_ir_enhanced tail
  | BinOp (IR_Add, dest, src, Imm 0) :: tail | BinOp (IR_Sub, dest, src, Imm 0) :: tail ->
    Move (dest, src) :: optimize_ir_enhanced tail
  | BinOp (IR_Mul, dest, src, Imm 1) :: tail | BinOp (IR_Div, dest, src, Imm 1) :: tail ->
    Move (dest, src) :: optimize_ir_enhanced tail
  | BinOp (IR_Mul, dest, _, Imm 0) :: tail -> Li (dest, 0) :: optimize_ir_enhanced tail
  (* 新增模式 *)
  | Move (dest, src) :: tail when dest = src -> optimize_ir_enhanced tail
  | Li (reg1, n1) :: BinOp (IR_Add, dest, r1, Imm n2) :: tail when reg1 = r1 ->
    Li (dest, n1 + n2) :: optimize_ir_enhanced tail
  | Store (reg, Stack offset1) :: Load (same_reg, Stack offset2) :: tail
    when reg = same_reg && offset1 = offset2 -> optimize_ir_enhanced tail
  | head :: tail -> head :: optimize_ir_enhanced tail
  | [] -> []
;;

(* 增强版的程序优化入口 *)
let optimize_program_enhanced (p : program) : program =
  List.map (fun f -> { f with body = List.map optimize_stmt_enhanced f.body }) p
;;

(* 多轮IR优化 *)
let optimize_ir_multipass (ir_code : ir list) : ir list =
  let rec fix_point prev_ir =
    let new_ir = optimize_ir_enhanced prev_ir in
    if new_ir = prev_ir then new_ir else fix_point new_ir
  in
  fix_point ir_code
;;
