(* lib/AstToIR.ml  *)
open Ast
open Ir
open Optimize

(* --- 模块与类型定义 --- *)
module Enwli = Map.Make (String)

(* 管理变量作用域的栈 *)
module Estack = struct
  type t = operand Enwli.t list

  let empty : t = [ Enwli.empty ]
  (* 进入新作用域 *)
  let enter (stk : t) : t = Enwli.empty :: stk
  (* 退出作用域 *)
  let exit = function
    | _ :: tl -> tl
    | [] -> failwith "Estack.exit: empty stack"
  ;;
  (* 在当前作用域中添加一个变量映射 *)
  let add name v = function
    | top :: tl -> Enwli.add name v top :: tl
    | [] -> failwith "Estack.add: empty stack"
  ;;
  (* 从当前作用域开始，向外层作用域递归查找一个变量的定义 *)
  let rec l_up name = function
    | [] -> failwith ("unbound variable: " ^ name)
    | m :: ms ->
      (match Enwli.find_opt name m with
       | Some v -> v
       | None -> l_up name ms)
  ;;
end


(* 上下文记录，用于在递归翻译时传递状态，
如当前函数名、变量作用域栈、以及循环中 break 和 continue 的目标标签 。 *)
type text = {
  func_name : string;
  e_stack : Estack.t ref;
  breakb : string option;
  continueb : string option;
}

module S_set = Set.Make (String)(*收集和分析在代码中使用的变量名 *)

(* 递归地检查一个expr是否具有副作用。
目前只认为函数调用 (Ast.Call) 具有副作用 。 *)
let rec side_effecte = function
  | Ast.Call _ -> true
  | Ast.UnOp (_, e) -> side_effecte e
  | Ast.BinOp (_, l, r) -> side_effecte l || side_effecte r
  | _ -> false
;;

(* 递归地检查一个stmt是否具有副作用。 *)
let rec side_effects = function
  | Ast.Expr e -> side_effecte e
  | Ast.Return _ -> true
  | Ast.Break | Ast.Continue -> true
  | Ast.If (_, t, f) ->
    side_effects t
    || (match f with
        | Some f -> side_effects f
        | None -> false)
  | Ast.While (_, b) -> side_effects b
  | Ast.Block ss -> List.exists side_effects ss
  | _ -> false
;;

(* 递归地检查一个语句列表（stmts）中是否包含 While 循环 。 *)
let rec c_while = function
  | [] -> false
  | Ast.While _ :: _ -> true
  | Ast.Block ss :: rest -> c_while ss || c_while rest
  | _ :: rest -> c_while rest
;;

(* 检查一个给定的变量名 (name) 是否在某个语句 (stmt) 中被使用 *)
let rec uvar_stmt name stmt =
  let rec uvar_expr name = function
    | Id id -> id = name
    | BinOp (_, e1, e2) -> uvar_expr name e1 || uvar_expr name e2
    | UnOp (_, e) -> uvar_expr name e
    | Call (_, args) -> List.exists (uvar_expr name) args
    | _ -> false
  in
  match stmt with
  | Ast.Assign (_, e) -> uvar_expr name e
  | VarDecl (_, Some e) -> uvar_expr name e
  | Exprt e -> uvar_expr name e
  | If (cond, s1, s2_opt) ->
    uvar_expr name cond
    || uvar_stmt name s1
    || (match s2_opt with
        | Some s2 -> uvar_stmt name s2
        | None -> false)
  | While (cond, s) -> uvar_expr name cond || uvar_stmt name s
  | Block ss -> List.exists (uvar_stmt name) ss
  | _ -> false
;;

(* 收集并返回一个语句stmt中所有被使用的变量名的集合 (S_set)  *)
let rec vars_stmt =
  let rec vars_expr = function
    | Ast.Id x -> S_set.singleton x
    | Ast.UnOp (_, e) -> vars_expr e
    | Ast.BinOp (_, l, r) -> S_set.union (vars_expr l) (vars_expr r)
    | Ast.Call (_, args) ->
      List.fold_left (fun acc e -> S_set.union acc (vars_expr e)) S_set.empty args
    | _ -> S_set.empty
  in
  function
  | Ast.Expr e -> vars_expr e
  | Ast.Return (Some e) -> vars_expr e
  | Ast.Return None -> S_set.empty
  | Ast.VarDecl (_, Some e) -> vars_expr e
  | Ast.Assign (_, e) -> vars_expr e
  | Ast.If (c, t, f) ->
    S_set.union
      (vars_expr c)
      (S_set.union
         (vars_stmt t)
         (match f with
          | Some f -> vars_stmt f
          | None -> S_set.empty))
  | Ast.While (c, b) -> S_set.union (vars_expr c) (vars_stmt b)
  | Ast.Block ss ->
    List.fold_left (fun acc s -> S_set.union acc (vars_stmt s)) S_set.empty ss
  | _ -> S_set.empty
;;

(* 死循环消除 *)
(* 移除那些没有副作用（如函数调用）且其修改的变量在循环外不会被再次读取的 while 循环  *)
let rec r_while stmts =
  let rec go acc = function
    | [] -> List.rev acc
    | stmt :: rest ->
      let keep1 =
        match stmt with
        | Ast.While (_, body) ->
          let se = side_effects body in
          let writes = vars_stmt body in
          let future_reads =
            List.fold_left (fun s st -> S_set.union s (vars_stmt st)) S_set.empty rest
          in
          not (not se && S_set.is_empty (S_set.inter writes future_reads))
        | Ast.Block _ -> true
        | _ -> true
      in
      let acc1 =
        if keep1 then
          let stmt' =
            match stmt with
            | Ast.Block ss -> Ast.Block (r_while ss)
            | other -> other
          in
          stmt' :: acc
        else acc
      in
      go acc1 rest
  in
  go [] stmts
;;

(* 对一整个编译单元中的每个函数应用r_while 优化  *)
let pre_ast (pro : Ast.program) : Ast.program =
  List.map (fun f -> if c_while f.Ast.body then { f with Ast.body = r_while f.body } else f) pro
;;

(* ----------特定循环模式消除---------- *)

(* 识别并删除简单的自增循环，将其替换为空块  *)
let rec tri_self_loop stmt =
  match stmt with
  | While (BinOp (Lt, Id var, IntLiteral _), Block [ Assign (var2, BinOp (Add, Id var3, IntLiteral 1)) ])
    when var = var2 && var = var3 -> Block []
  | Block stmts -> Block (stmts |> List.map tri_self_loop |> List.filter (function Block [] -> false | _ -> true))
  | If (cond, s1, s2_opt) -> If (cond, tri_self_loop s1, Option.map tri_self_loop s2_opt)
  | While (cond, Block body) -> While (cond, Block (List.map tri_self_loop body))
  | other -> other

(* 循环展开与强度削减 *)
(* 识别特定的嵌套循环模式，并将内层循环的累加操作转换成一次乘法运算 *)
let rec el_loop (stmt : stmt) : stmt =
  match stmt with
  | While (BinOp (Lt, Id idx, IntLiteral n), Block body) ->
    let match_loop stmts =
      match stmts with
      | ValDecl (k_name, Some (IntLiteralr 0)) :: assigns_before
        when List.exists (function Ast.Assign (_, IntLiteral 0) -> true | _ -> false) assigns_before ->
        let a_inst, rest = List.partition (function Ast.Assign (_, IntLiteral 0) -> true | _ -> false) assigns_before in
        let acc_names = List.filter_map (function Ast.Assign (name, IntLiteral 0) -> Some name | _ -> None) a_inst in
        (match rest with
         | While (BinOp (Lt, Id k_id, IntLiteral m), Block inner_body) :: tail when k_id = k_name ->
           let vaild_expr =
             List.filter_map
               (function
                 | Ast.Assign (acc, BinOp (Add, Id acc2, expr)) when acc = acc2 && List.mem acc acc_names -> Some (acc, expr)
                 | _ -> None)
               inner_body
           in
           let valid = List.length vaild_expr = List.length acc_names && List.for_all (fun acc -> List.mem acc acc_names) (List.map fst vaild_expr) in
           if valid then Some (k_name, m, vaild_expr, tail) else None
         | _ -> None)
      | _ -> None
    in
    (match match_loop body with
     | Some (k_var, m, acc_exprs, tail_after_loop) ->
       let new_accs = List.map (fun (acc, expr) -> Ast.Assign (acc, BinOp (Mul, expr, IntLiteral m))) acc_exprs in
       let k_decl = if List.exists (uvar_stmt k_var) tail_after_loop then [ ValDecl (k_var, Some (IntLiteral 0)) ] else [] in
       let new_body = Block (k_decl @ new_accs @ List.map el_loop tail_after_loop) in
       While (BinOp (Lt, Id idx, IntLiteral n), new_body)
     | None -> While (BinOp (Lt, Id idx, IntLiteral n), Block (List.map el_loop body)))
  | While (cond, Block body) -> While (cond, Block (List.map el_loop body))
  | Block stmts -> Block (List.map el_loop stmts)
  | If (cond, t_branch, f_branch) -> If (cond, el_loop t_branch, Option.map el_loop f_branch)
  | _ -> stmt
;;

 (* 将 el_loop 和 tri_self_loop 优化应用于单个函数体 *)
let el_loopfunc (f : func_def) : func_def =
  let new_body = f.body |> List.map el_loop |> List.map tri_self_loop in
  { f with body = new_body }
;;
(* 将 el_loopfunc 应用于整个程序的所有函数  *)
let loop_elim_ast (pro : program) : program = List.map el_loopfunc pro


(* --------------IR 生成与辅助函数--------------- *)
module LabelMap = Map.Make (String)

(* 生成一个唯一的临时寄存器 *)
let temp_id = ref 0
let fr_temp () =
  incr temp_id;
  Reg ("t" ^ string_of_int !temp_id)

(* 为一个基础名字（base）生成一个唯一的内部名称 *)
let name_id = ref 0
let fr_name base =
  incr name_id;
  base ^ "_" ^ string_of_int !name_id

(* 生成一个唯一的标签字符串 *)
let la_id = ref 0
let ir_label_id = ref 0
let fr_label () =
  incr la_id;
  "L" ^ string_of_int !la_id

(* 为一个高级标签（param）查找或创建一个唯一的 IR 级标签（如 "LABEL1"），并维护一个从高级标签到 IR 标签的映射表 。 *)
let frir_label (label_map : string LabelMap.t) (l : param) : string * string LabelMap.t =
  match LabelMap.find_opt l label_map with
  | Some lbl -> lbl, label_map
  | None ->
    let id = !ir_label_id in
    incr ir_label_id;
    let lbl = "LABEL" ^ string_of_int id in
    let label_map' = LabelMap.add l lbl label_map in
    lbl, label_map'

(* 转换成字符串表示 *)
let string_of_unop = function
  | Not -> "!" | Plus -> "+" | Minus -> "-"
let string_of_binop = function
  | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "%"
  | Eq -> "==" | Neq -> "!=" | Less -> "<" | Leq -> "<=" | Greater -> ">"
  | Geq -> ">=" | Land -> "&&" | Lor -> "||"


(* ===============AST 到 IR 核心翻译============= *)

(* 表达式翻译：递归地将一个 AST 表达式（expr）翻译成 IR 指令。
它会为子表达式的结果生成临时寄存器，并返回最终结果所在的 operand 和一系列新生成的指令 。 *)
let rec expr_ir (ctx : text) (e : expr) : operand * inst_r list =
  match e with
  | IntLiteral n -> (Imm n, [])
  | Id name ->
    let operand = Estack.l_up name !(ctx.e_stack) in
    (operand, [])
  | UnOp (op, e1) ->
    let operand, code = expr_ir ctx e1 in
    let res = fr_temp () in
    (res, code @ [ UnOp (string_of_unop op, res, operand) ])
  | BinOp (op, e1, e2) ->
    let lhs, c1 = expr_ir ctx e1 in
    let rhs, c2 = expr_ir ctx e2 in
    (match op with
     | Land | Lor ->
       let dst = fr_temp () in
       (dst, c1 @ c2 @ [ BinOp (string_of_binop op, dst, lhs, rhs) ])
     | _ ->
       let dst = fr_temp () in
       (dst, c1 @ c2 @ [ BinOp (string_of_binop op, dst, lhs, rhs) ]))
  | Call (f, args) ->
    let arg_op_pairs = List.map (expr_ir ctx) args in
    let oprs, codes_list = List.split arg_op_pairs in
    let codes = List.concat codes_list in
    let ret = fr_temp () in
    (ret, codes @ [ Call (ret, f, oprs) ])

type stmt_res = Returned of inst_r list | Normal of inst_r list

let flatten = function Returned code | Normal code -> code
let always_returns = function Returned _ -> true | Normal _ -> false
let junp_return insts = match List.rev insts with Goto _ :: _ | Ret _ :: _ -> true | _ -> false

(* ------------布尔表达式规范化------------ *)
(* 使用德摩根定律等规则，将 ! 运算符尽可能地向表达式内部推，并转换关系运算符 *)
let rec nor_expr = function
  | Ast.UnOp (Not, UnOp (Not, e)) -> nor_expr e
  | UnOp (Not, BinOp (And, a, b)) -> nor_expr (BinOp (Or, UnOp (Not, a), UnOp (Not, b)))
  | UnOp (Not, BinOp (Or, a, b)) -> nor_expr (BinOp (And, UnOp (Not, a), UnOp (Not, b)))
  | UnOp (Not, BinOp (op, a, b)) ->
    let neg =
      match op with
      | Eq -> Neq | Neq -> Eq | Lt -> Ge | Le -> Gt | Gt -> Le | Ge -> Lt
      | _ -> failwith "unsupported negation of this binary op"
    in
    Ast.BinOp (neg, nor_expr a, nor_expr b)
  | BinOp (op, a, b) -> BinOp (op, nor_expr a, nor_expr b)
  | UnOp (op, a) -> UnoOp (op, nor_expr a)
  | Call (f, args) -> Call (f, List.map nor_expr args)
  | e -> e

(* 将复杂的逻辑与（&&）和逻辑或（||）表达式分解成嵌套的 if-then-else 结构，以便于后续翻译 *)
let rec des_stmt = function
  | If (cond, then_b, Some else_b) ->
    let cond = nor_expr cond in
    let ands = spand cond in
    if List.length ands > 1 then
      let rec nest_and = function
        | [ x ] -> If (x, then_b, Some else_b)
        | hd :: tl -> If (hd, Block [ nest_and tl ], Some else_b)
        | [] -> Block []
      in
      nest_and ands |> des_stmt
    else
      let ors = spor cond in
      if List.length ors > 1 then
        let rec nest_or = function
          | [ x ] -> If (x, then_b, Some else_b)
          | hd :: tl -> If (hd, then_b, Some (nest_or tl))
          | [] -> Block []
        in
        nest_or ors |> des_stmt
      else If (cond, des_stmt then_b, Some (des_stmt else_b))
  | If (cond, then_b, None) ->
    let cond = nor_expr cond in
    let ands = spand cond in
    if List.length ands > 1 then
      let rec nest_and = function
        | [ x ] -> If (x, then_b, None)
        | hd :: tl -> If (hd, Block [ nest_and tl ], None)
        | [] -> Block []
      in
      nest_and ands |> des_stmt
    else
      let ors = spor cond in
      if List.length ors > 1 then
        let rec nest_or = function
          | [ x ] -> If (x, then_b, None)
          | hd :: tl -> If (hd, then_b, Some (nest_or tl))
          | [] -> Block []
        in
        nest_or ors |> des_stmt
      else If (cond, des_stmt then_b, None)
  | While (cond, body) -> While (nor_expr cond, des_stmt body)
  | Block ss -> Block (List.map des_stmt ss)
  | other -> other

  (* 语句翻译：递归地将一个 AST 语句（stmt）翻译成 IR 指令 *)
let rec stmt_res (ctx : text) (in_tail : bool) (s : stmt) : stmt_res =
  match s with
  | Expr e ->
    let _, code = expr_ir ctx e in
    Normal code
  | ValDecl (x, None) ->
    let new_name = fr_name x in
    ctx.e_stack := Estack.add x (Var new_name) !(ctx.e_stack);
    Normal []
  | ValDecl (x, Some e) ->
    let v, c = expr_ir ctx e in
    let new_name = fr_name x in
    ctx.e_stack := Estack.add x (Var new_name) !(ctx.e_stack);
    Normal (c @ [ Assign (Var new_name, v) ])
  | Assign (x, e) ->
    let v, c = expr_ir ctx e in
    let var = Estack.l_up x !(ctx.e_stack) in
    Normal (c @ [ Assign (var, v) ])
  | Return None -> if in_tail then Returned [] else Returned [ Ret None ]
  | Return (Some e) ->
    (match e with
     | Call (f, args) when f = ctx.func_name ->
       let arg_op_pairs = List.map (expr_ir ctx) args in
       let arg_oprs, codes_list = List.split arg_op_pairs in
       let arg_codes = List.concat codes_list in
       Returned (arg_codes @ [ TailCall (f, arg_oprs) ])
     | _ ->
       let r, code = expr_ir ctx e in
       Returned (code @ [ Ret (Some r) ]))
  | If (cond, tstmt, Some fstmt) ->
    let cnd, cc = expr_ir ctx cond in
    let lthen = fr_label () and lelse = fr_label () and lend = fr_label () in
    let then_res = stmt_res ctx in_tail tstmt and else_res = stmt_res ctx in_tail fstmt in
    let raw_then = flatten then_res in
    let then_code = if junp_return raw_then then raw_then else raw_then @ [ Goto lend ] in
    let raw_else = flatten else_res in
    let else_code = if junp_return raw_else then raw_else else raw_else @ [ Goto lend ] in
    let code =
      cc @ [ IfGoto (cnd, lthen); Goto lelse ] @ [ Label lthen ] @ then_code
      @ [ Label lelse ] @ else_code @ [ Label lend ]
    in
    if always_returns then_res && always_returns else_res then Returned code else Normal code
  | If (cond, tstmt, None) ->
    let cnd, cc = expr_ir ctx cond in
    let lthen = fr_label () and lskip = fr_label () in
    let then_res = stmt_res ctx in_tail tstmt in
    let then_code = flatten then_res in
    let code = cc @ [ IfGoto (cnd, lthen); Goto lskip ] @ [ Label lthen ] @ then_code @ [ Label lskip ] in
    Normal code
  | While (cond, body) ->
    let lcond = fr_label () and lbody = fr_label () and lend = fr_label () in
    let ctx_loop = { ctx with breakb = Some lend; continueb = Some lcond } in
    let cnd, ccode = expr_ir ctx_loop cond in
    let body_res = stmt_res ctx_loop false body in
    let bcode = flatten body_res in
    let code =
      [ Goto lcond; Label lcond ] @ ccode @ [ IfGoto (cnd, lbody); Goto lend ]
      @ [ Label lbody ] @ bcode @ [ Goto lcond; Label lend ]
    in
    Normal code
  | Break -> (match ctx.breakb with Some lbl -> Normal [ Goto lbl ] | None -> failwith "break used outside loop")
  | Continue -> (match ctx.continueb with Some lbl -> Normal [ Goto lbl ] | None -> failwith "continue used outside loop")
  | Block stmts ->
    ctx.e_stack := Estack.enter !(ctx.e_stack);
    let rec loop acc = function
      | [] -> Normal acc
      | [ last ] ->
        (match stmt_res ctx in_tail last with
         | Returned c -> Returned (acc @ c)
         | Normal c -> Normal (acc @ c))
      | hd :: tl ->
        (match stmt_res ctx false hd with
         | Returned c -> Returned (acc @ c)
         | Normal c -> loop (acc @ c) tl)
    in
    let res = loop [] stmts in
    ctx.e_stack := Estack.exit !(ctx.e_stack);
    res


    (* ==================IR 结构化与优化==================== *)

  (* 构建控制流图 *)
  (* par_block 函数将线性的指令流根据 Label 和跳转指令切分成基本块（block_r），构建控制流图的第一步 *)
let par_block (insts : inst_r list) : block_r list =
  let rec split acc curr label label_map insts =
    match insts with
    | [] -> List.rev acc
    | Label l :: rest ->
      (match curr with
       | [] ->
         let next_label, label_map' = frir_label label_map l in
         split acc [ Label l ] next_label label_map' rest
       | _ ->
         let next_label, label_map' = frir_label label_map l in
         let blk = { label; insts = List.rev curr; terminator = TermSeq next_label; preds = []; succs = []; l_in = OperandSet.empty; l_out = OperandSet.empty } in
         let acc1 = blk :: acc in
         split acc1 [ Label l ] next_label label_map' rest)
    | Goto l :: rest ->
      let goto_label, label_map' = frir_label label_map l in
      let next_label, label_map'' = frir_label label_map' ("__blk" ^ string_of_int !ir_label_id) in
      let blk = { label; insts = List.rev (Goto l :: curr); terminator = TermGoto goto_label; preds = []; succs = []; l_in = OperandSet.empty; l_out = OperandSet.empty } in
      split (blk :: acc) [] next_label label_map'' rest
    | IfGoto (cond, l) :: rest ->
      let then_label, label_map' = frir_label label_map l in
      let else_label, label_map'' = frir_label label_map' ("__else" ^ string_of_int !ir_label_id) in
      let blk = { label; insts = List.rev (IfGoto (cond, l) :: curr); terminator = TermIf (cond, then_label, else_label); preds = []; succs = []; l_in = OperandSet.empty; l_out = OperandSet.empty } in
      split (blk :: acc) [] else_label label_map'' rest
    | Ret op :: rest ->
      let next_label, label_map' = frir_label label_map ("__ret" ^ string_of_int !ir_label_id) in
      let blk = { label; insts = List.rev (Ret op :: curr); terminator = TermRet op; preds = []; succs = []; l_in = OperandSet.empty; l_out = OperandSet.empty } in
      split (blk :: acc) [] next_label label_map' rest
    | inst :: rest -> split acc (inst :: curr) label label_map rest
  in
  let entry_label, label_map = frir_label LabelMap.empty "entry" in
  split [] [] entry_label label_map insts





(* ----------IR 优化函数-------- *)
(* 检查一条 IR 指令是否具有副作用 *)
let has_effect inst =
  match inst with
  | Call _ | Store _ | Ret _ -> true
  | Goto _ | IfGoto _ | Label _ -> true
  | _ -> false

module E_map = Map.Make (struct
  type t = string * operand * operand
  let compare = compare
end)

(* 执行公共子表达式消除 *)
(* 缓存二元运算的结果，并在后续遇到相同运算时重用结果 *)
let cse_block (blk : block_r) : block_r =
  let expr_table = ref E_map.empty in
  let new_insts =
    List.fold_left
      (fun acc inst ->
         match inst with
         | Binop (op, dst, lhs, rhs) ->
           let key = op, lhs, rhs in
           (match E_map.find_opt key !expr_table with
            | Some prev -> Assign (dst, prev) :: acc
            | None ->
              expr_table := E_map.add key dst !expr_table;
              inst :: acc)
         | _ ->
           let kills_all = match inst with | Store _ | Call _ | TailCall _ | Ret _ -> true | _ -> false in
           if kills_all then expr_table := E_map.empty;
           inst :: acc)
      []
      blk.insts
    |> List.rev
  in
  { blk with insts = new_insts }

  (* 执行死代码消除 *)
  (* 首先运行存活变量分析，然后反向遍历基本块，移除那些没有副作用且其定义（写入）的变量在之后不被使用的指令 。 *)
let dcode_elim blocks (print_l : bool) =
  run_liveness_analysis blocks print_l;
  List.map
    (fun blk ->
       let live = ref blk.l_out in
       let new_insts =
         List.fold_right
           (fun inst acc ->
              let def, use = get_def_use_sets_for_instruction inst in
              let must_keep = has_effect inst in
              let def_is_live =
                (not (OperandSet.is_empty def))
                && OperandSet.exists (fun v -> OperandSet.mem v !live) def
              in
              if must_keep || def_is_live then (
                live := OperandSet.union use (OperandSet.diff !live def);
                inst :: acc)
              else acc)
           blk.insts
           []
       in
       { blk with insts = new_insts })
    blocks

module O_map = Map.Make (struct
  type t = operand
  let compare = compare
end)

let rec res_copy env op =
  match op with
  | Var _ | Reg _ ->
    (match O_map.find_opt op env with
     | Some v when v <> op -> res_copy env v
     | _ -> op)
  | _ -> op

(* 执行拷贝传播 *)
(* 将形如 y = x 的赋值记录下来，并用 x 替换后续对 y 的使用，同时移除冗余的赋值指令  *)
let copy_block (blk : block_r) : block_r =
  let copy_env = ref O_map.empty in
  let propagate_op op = res_copy !copy_env op in
  let new_insts =
    List.filter_map
      (fun inst ->
         match inst with
         | Assign (dst, src) ->
           let src' = propagate_op src in
           if dst = src' then None
           else (copy_env := O_map.add dst src' !copy_env; Some (Assign (dst, src')))
         | Binop (op, dst, lhs, rhs) ->
           let lhs' = propagate_op lhs and rhs' = propagate_op rhs in
           copy_env := O_map.remove dst !copy_env; Some (Binop (op, dst, lhs', rhs'))
         | Unop (op, dst, src) ->
           let src' = propagate_op src in
           copy_env := O_map.remove dst !copy_env; Some (Unop (op, dst, src'))
         | Load (dst, src) ->
           let src' = propagate_op src in
           copy_env := O_map.remove dst !copy_env; Some (Load (dst, src'))
         | Store (dst, src) ->
           let dst' = propagate_op dst and src' = propagate_op src in
           Some (Store (dst', src'))
         | Ret (Some src) -> Some (Ret (Some (propagate_op src)))
         | Call (dst, fname, args) ->
           let args' = List.map propagate_op args in
           copy_env := O_map.remove dst !copy_env; Some (Call (dst, fname, args'))
         | TailCall (fname, args) -> Some (TailCall (fname, List.map propagate_op args))
         | (Goto _ | IfGoto _ | Label _ | Ret None) as i -> Some i)
      blk.insts
  in
  { blk with insts = new_insts }

  (* ！！优化版本 *)
  (* pipeline：翻译成线性 IR -> 构建 CFG -> 执行死代码消除 -> 公共子表达式消除 -> 拷贝传播 -> 再次死代码消除 。 *)
let func_iro (f : func_def) (print_l : bool) : ir_func_o =
  let des_body = match des_stmt (Block f.body) with Block ss -> ss | _ -> f.body in
  let f' = { f with body = des_body } in
  let i_env = List.fold_left (fun m p -> Enwli.add p (Var p) m) Enwli.empty f'.params in
  let ctx0 = { func_name = f'.fname; e_stack = ref [ i_env ]; breakb = None; continueb = None } in
  let raw_code =
    try stmt_res ctx0 false (Block f'.body) |> flatten with
    | e ->
      Printf.eprintf "Error generating IR for %s: %s\n" f'.fname (Printexc.to_string e);
      raise e
  in
  let en_lab = "entry_" ^ f'.fname in
  let tail_elim_ir = ref [ Label en_lab ] in
  List.iter
    (fun inst ->
       match inst with
       | TailCall (fname, args) when fname = f'.fname ->
         let assigns = List.mapi (fun i arg -> let param = List.nth f'.params i in Assign (Var param, arg)) args in
         tail_elim_ir := !tail_elim_ir @ assigns @ [ Goto en_lab ]
       | _ -> tail_elim_ir := !tail_elim_ir @ [ inst ])
    raw_code;
  let raw_blocks = par_block !tail_elim_ir in
  let cfg_blocks = Cfg.build_cfg raw_blocks in
  let opt_blocks = Cfg.optimize_cfg cfg_blocks in
  let dce_blocks = dcode_elim opt_blocks print_l in
  let cse_blocks = List.map cse_block dce_blocks in
  let copy_blocks = List.map copy_block cse_blocks in
  let final_blocks = dcode_elim copy_blocks print_l in
  { name = f'.fname; args = f'.params; blocks = final_blocks }

let rec opt_stmt stmt =
  match stmt with
  | While (cond, body) -> While (cond, opt_stmt body)
  | Block slist -> Block (List.map opt_stmt slist)
  | If (e, s1, Some s2) -> If (e, opt_stmt s1, Some (opt_stmt s2))
  | If (e, s1, None) -> If (e, opt_stmt s1, None)
  | _ -> stmt

let opt_func (f : func_def) : func_def = { f with body = List.map opt_stmt f.body }
let opt (pro : program) : program = List.map opt_func pro

(* ========== 步骤 2: 优化点 ========== *)
let translate_program_to_ir (pro : program) (optimize_flag : bool) (print_l : bool) : ir_program =
  (* 之前: 优化流程分两步，并且重复绑定变量 pro1 *)
  (* let pro1 = if optimize_flag then pro |> pre_ast |> loop_elim_ast |> opt |> loop_elim_ast else pro in
  let pro1 = opt pro1 in *)
  
  (* 之后: 将所有优化步骤合并成一个清晰的流水线 *)
  let processed_pro =
    if optimize_flag then
      pro
      |> pre_ast
      |> loop_elim_ast
      |> opt
      |> loop_elim_ast
      |> opt
    else
      pro
  in
  if optimize_flag
  then Ir_funcs_o (List.map (fun f -> func_iro f print_l) processed_pro)
