open Ast
open Ir
(* ========== 打印 AST ========== *)

let string_of_typ = function TInt -> "int" | TVoid -> "void"

let string_of_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Eq -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_unop = function Not -> "!"  | Neg -> "-"

let rec string_of_expr = function
  | IntLiteral n -> Printf.sprintf "IntLiteral(%d)" n
  | Id s -> Printf.sprintf "Id(%s)" s
  | Call (fname, args) ->
      Printf.sprintf "Call(%s, [%s])" fname
        (String.concat "; " (List.map string_of_expr args))
  | BinOp (op, e1, e2) ->
      Printf.sprintf "BinOp(%s, %s, %s)" (string_of_binop op)
        (string_of_expr e1) (string_of_expr e2)
  | UnOp (op, e) ->
      Printf.sprintf "UnOp(%s, %s)" (string_of_unop op) (string_of_expr e)

let rec string_of_stmt = function
  | Block stmts ->
      Printf.sprintf "Block([\n  %s\n])"
        (String.concat ";\n  " (List.map string_of_stmt stmts))
  (* | Empty -> "Empty" *)
  | Expr e -> Printf.sprintf "Expr(%s)" (string_of_expr e)
  | Assign (id, e) -> Printf.sprintf "Assign(%s, %s)" id (string_of_expr e)
  | VarDecl (id, None) -> Printf.sprintf "VarDecl(%s)" id
  | VarDecl (id, Some e) -> Printf.sprintf "VarDecl(%s, %s)" id (string_of_expr e)
  | If (cond, s1, None) ->
      Printf.sprintf "If(%s, %s)" (string_of_expr cond) (string_of_stmt s1)
  | If (cond, s1, Some s2) ->
      Printf.sprintf "If(%s, %s, %s)" (string_of_expr cond) (string_of_stmt s1)
        (string_of_stmt s2)
  | While (cond, body) ->
      Printf.sprintf "While(%s, %s)" (string_of_expr cond) (string_of_stmt body)
  | Break -> "Break"
  | Continue -> "Continue"
  | Return None -> "Return"
  | Return (Some e) -> Printf.sprintf "Return(%s)" (string_of_expr e)

let string_of_func_def f =
  Printf.sprintf "Function %s(%s) : %s {\n%s\n}" f.fname
    (String.concat ", " f.params)
    (string_of_typ f.rettyp)
    (String.concat "\n" (List.map (fun s -> "  " ^ string_of_stmt s) f.body))

let string_of_program (unit : program) : string =
  String.concat "\n\n" (List.map string_of_func_def unit)


(* ========== 打印 IR ========== *)

let string_of_operand = function
  | Reg name -> "Reg " ^ name
  | Imm i -> "Imm " ^ string_of_int i
  | Var name -> "Var " ^ name

let string_of_operands ops = String.concat ", " (List.map string_of_operand ops)

let string_of_ir_inst = function
  | Binop (op, dst, lhs, rhs) ->
      Printf.sprintf "%s = %s %s %s" (string_of_operand dst)
        (string_of_operand lhs) op (string_of_operand rhs)
  | Unop (op, dst, src) ->
      Printf.sprintf "%s = %s%s" (string_of_operand dst) op
        (string_of_operand src)
  | Load (dst, src) ->
      Printf.sprintf "%s = *%s" (string_of_operand dst) (string_of_operand src)
  | Store (dst, src) ->
      Printf.sprintf "*%s = %s" (string_of_operand dst) (string_of_operand src)
  | Goto label -> Printf.sprintf "goto %s" label
  | IfGoto (cond, label) ->
      Printf.sprintf "if %s goto %s" (string_of_operand cond) label
  | Label name -> Printf.sprintf "%s:" name
  | Call (ret, fname, args) ->
      Printf.sprintf "%s = call %s(%s)" (string_of_operand ret) fname
        (string_of_operands args)
  | Ret None -> "return"
  | Ret (Some op) -> Printf.sprintf "return %s" (string_of_operand op)
  | Assign (dst, src) ->
      Printf.sprintf "%s = %s" (string_of_operand dst) (string_of_operand src)
  (* —— 新增 —— *)
  | TailCall (fname, args) ->
      (* 打印成“tailcall f(arg1,arg2,…)” *)
      Printf.sprintf
        "tailcall %s(%s)"
        fname
        (String.concat ", " (List.map string_of_operand args))

let string_of_ir_term = function
  | TermGoto l -> Printf.sprintf "  terminator: goto %s" l
  | TermIf (cond, l1, l2) ->
      Printf.sprintf "  terminator: if %s goto %s else goto %s"
        (string_of_operand cond) l1 l2
  | TermRet None -> "  terminator: return"
  | TermRet (Some op) ->
      Printf.sprintf "  terminator: return %s" (string_of_operand op)
  | TermSeq l-> Printf.sprintf "  terminator: seqence %s" l

let print_ir_func oc (f : func_r) =
  Printf.fprintf oc "function %s(%s):\n" f.name (String.concat ", " f.args);
  List.iter (fun inst -> Printf.fprintf oc "  %s\n" (string_of_ir_inst inst)) f.body

let print_ir_block oc (b : block_r) =
  Printf.fprintf oc "%s:\n" b.label;
  List.iter
    (fun inst -> Printf.fprintf oc "    %s\n" (string_of_ir_inst inst))
    b.insts;
  Printf.fprintf oc "%s\n" (string_of_ir_term b.terminator);
  Printf.fprintf oc "    preds: [%s]\n" (String.concat ", " b.preds);
  Printf.fprintf oc "    succs: [%s]\n" (String.concat ", " b.succs)

let print_ir_func_o oc (f : ir_func_o) =
  Printf.fprintf oc "function %s(%s):\n" f.name (String.concat ", " f.args);
  List.iter (print_ir_block oc) f.blocks

let print_ir_program oc (prog : ir_program) =
  match prog with
  | Ir_funcs funcs -> List.iter (print_ir_func oc) funcs
  | Ir_funcs_o funcs -> List.iter (print_ir_func_o oc) funcs
