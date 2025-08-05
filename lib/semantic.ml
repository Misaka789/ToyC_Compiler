open Ast

(* 符号表类型 *)
module VarEnv = Map.Make(String)
type func_sig = { params: typ list; return: typ }
module FuncEnv = Map.Make(String)

(* **FIX**: Define a module for string sets using the Set.Make functor *)
module StringSet = Set.Make(String)

(* 新类型：存储全局函数表 + 各函数的局部变量表 *)
type analysis_result = {
  global_funcs : func_sig FuncEnv.t;  (* 全局函数签名 *)
  local_vars  : (string * typ VarEnv.t) list;  (* 函数名 -> 其局部变量 *)
}

(* 构建全局函数符号表 *)
let build_func_env (funcs: func_def list) : func_sig FuncEnv.t =
  List.fold_left (fun acc (f:func_def) ->
    let param_types = List.map (fun _ -> TInt) f.params in
    FuncEnv.add f.fname { params = param_types; return = f.rettyp } acc
  ) FuncEnv.empty funcs

(* **FIX**: This function now ONLY analyzes local variables, not parameters. *)
let analyze_local_vars (f: func_def) : typ VarEnv.t =
  (* Create a set of parameter names for quick lookup *)
  (* **FIX**: Use the correctly defined StringSet module *)
  let param_set = f.params |> List.to_seq |> StringSet.of_seq in

  (* 递归分析语句 *)
  let rec analyze_stmt env = function
    | Expr _ -> env
    | VarDecl (name, _) ->
        (* **FIX**: Use the correctly defined StringSet module *)
        if VarEnv.mem name env || StringSet.mem name param_set then
          failwith ("Duplicate variable definition: " ^ name)
        else
          VarEnv.add name TInt env
    | If (_, s1, opt_s2) ->
        let env1 = analyze_stmt env s1 in
        (match opt_s2 with Some s2 -> analyze_stmt env1 s2 | None -> env1)
    | While (_, body) -> analyze_stmt env body
    | Block stmts -> List.fold_left analyze_stmt env stmts
    | _ -> env  (* Break, Continue, Return *)
  in

  (* Start with an empty environment for local variables *)
  List.fold_left analyze_stmt VarEnv.empty f.body

(* 程序入口：返回全局函数表 + 所有函数的局部变量表 *)
let analyze_program (p: program) : analysis_result =
  let global_funcs = build_func_env p in
  let local_vars =
    List.map (fun f ->
      (f.fname, analyze_local_vars f)
    ) p
  in
  { global_funcs; local_vars }