(* lib/ast.ml *)
(* 定义了抽象语法树（AST）的数据结构，用于表示程序的语法结构。 *)




(* 基本类型定义 *)
type typ =
  | TInt
  (* 整数类型 *)
  | TVoid (* 空类型 *)

(* 定义所有二元运算符 *)
type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge
  | And
  | Or

(* 定义所有一元运算符 *)
type unop =
  | Neg
  | Not
  | Plus
(* type unop = Not | Plus | Neg *)

(* 表达式 (Expr) 的定义 *)
type expr =
  | IntLiteral of int (* NUMBER *) (* 整数字面量 *)
  | Id of string (* ID *)
  | BinOp of binop * expr * expr (* ... op ... *)
  | UnOp of unop * expr (* + - ! *)
  | Call of string * expr list (* ID(Expr, ...) *)

(* 语句 (Stmt) 的定义 *)
type stmt =
  | Block of stmt list
  | Expr of expr
  | Return of expr option
  | If of expr * stmt * stmt option (* if (expr) stmt (else stmt)? *)
  | While of expr * stmt
  (* Assign移到stmt *)
  | Assign of string * expr (*赋值 ID = Expr *)
  | VarDecl of string * expr option(* 变量声明 int ID = Expr; *)
  | Break     (* 跳出循环 *)
  | Continue  (* 跳过当前循环迭代 *)

(* 形参 (Param) 的定义: int ID *)
(* 我们只支持 int，所以只需要名字 *)
type param = string

(* 函数定义 (FuncDef) *)
(*函数名、参数列表、返回值(int/void)、函数体*)
type func_def =                     (*eg:*)
  { fname : string                  (*add*)
  ; params : param list             (*[x;y]*)
  ; rettyp : typ (* 明确返回类型 *)  (*TInt/Tvoid*)
  ; body : stmt list                (*[Expr (BinOp (Add, Id "x", Id "y"))]*)
  }

(* 编译单元 (CompUnit)，也就是整个程序 *)
type program = func_def list (* FuncDef+ 一个程序由多个函数定义组成*)

let indent level = String.make (level * 2) ' '
(* indent 函数用于生成一个由 level 个空格组成的字符串，用于代码缩进 *)

(* 将二元运算符转换为字符串 *)
let string_of_binop = function
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | Mod -> "Mod"
  | Eq -> "Eq"
  | Neq -> "Neq"
  | Lt -> "Lt"
  | Le -> "Le"
  | Gt -> "Gt"
  | Ge -> "Ge"
  | And -> "And"
  | Or -> "Or"
;;

(* 将一元运算符转换为字符串 *)
let string_of_unop = function
  | Not -> "Not"
  | Neg -> "Neg"
;;

(* 递归打印表达式 *)
let rec string_of_expr e =
  match e with
  | IntLiteral n -> Printf.sprintf "Int(%d)" n
  | Id s -> Printf.sprintf "Id(%s)" s
  | BinOp (op, e1, e2) ->
    Printf.sprintf
      "BinOp(%s, %s, %s)"
      (string_of_binop op)
      (string_of_expr e1)
      (string_of_expr e2)
  | UnOp (op, e) -> Printf.sprintf "UnOp(%s, %s)" (string_of_unop op) (string_of_expr e)
  | Assign (id, e) -> Printf.sprintf "Assign(%s, %s)" id (string_of_expr e)
  | Call (fname, args) ->     (*expr list*)
    let args_str = List.map string_of_expr args |> String.concat ", " in
    Printf.sprintf "Call(%s, [%s])" fname args_str

and string_of_stmt s =
  match s with
  | Block stmts ->   (*stmt list*)
    let stmts_str = List.map string_of_stmt stmts |> String.concat "; " in
    Printf.sprintf "Block([%s])" stmts_str
  | Expr e -> Printf.sprintf "Expr(%s)" (string_of_expr e)  (*eg:输出：Expr(BinOp(Add, Int(1), Int(2)))*)
  | Return expr_opt ->
    let inner_str =
      match expr_opt with
      | Some e -> Printf.sprintf "Some(%s)" (string_of_expr e)
      | None -> "None"
    in
    Printf.sprintf "Return(%s)" inner_str       (*eg:输出：Return(Some(Int(42)))*)
  | If (cond, then_stmt, else_stmt_opt) ->
    let else_str =
      match else_stmt_opt with
      | Some else_stmt -> Printf.sprintf ", Else(%s)" (string_of_stmt else_stmt)
      | None -> ""
    in
    Printf.sprintf
      "If(%s, %s%s)"
      (string_of_expr cond)
      (string_of_stmt then_stmt)
      else_str      (*eg:  If(BinOp(Eq, Id(x), Int(0)), Break, Else(Continue))*)
  | While (cond, body) ->
    Printf.sprintf "While(%s, %s)" (string_of_expr cond) (string_of_stmt body)
    (*eg: While(BinOp(Lt, Id(i), Int(10)), Expr(BinOp(Add, Id(i), Int(1))))*)
  | VarDecl (id, e) -> Printf.sprintf "VarDecl(%s, %s)" id (string_of_expr e)
  | Break -> "Break"
  | Continue -> "Continue"
;;

let string_of_func_def f =
  let params_str = String.concat ", " f.params in
  (* 将 body 语句列表直接转换为一个 Block 字符串，更符合结构 *)
  let body_str = string_of_stmt (Block f.body) in
  Printf.sprintf "FuncDef(name=%s, params=[%s], body=%s)" f.fname params_str body_str
;;   (*eg: FuncDef(name=add, params=[x, y], body=Block([Expr(BinOp(Add, Id(x), Id(y)))]))*)


let string_of_program prog =
  let funcs_str = List.map string_of_func_def prog |> String.concat ", " in
  Printf.sprintf "[%s]" funcs_str
;;  (*eg: [FuncDef(...), FuncDef(...)] 见上个函数*)
