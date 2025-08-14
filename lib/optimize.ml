(* lib/optimize.ml *)
(* 优化寄存器分配————实现活性分析*)
(* 通过数据流分析确定程序中每个基本块的活跃变量集合。 *)

open Ir

(* 辅助函数，打印。 *)
let pri_oper op =
  match op with
  | Var v -> v
  | Reg r -> r
  | Imm i -> string_of_int i

let pri_info blocks =
  List.iter
    (fun blk ->
      Printf.printf "Block: %s\n" blk.label;
      Printf.printf "  live_in: {%s}\n"
        (String.concat ", " (List.map pri_oper (OperandSet.elements blk.l_in)));
      Printf.printf "  live_out: {%s}\n"
        (String.concat ", " (List.map pri_oper (OperandSet.elements blk.l_out))))
    blocks



(* 计算单条指令的 def 和 use 集合，返回一个元组 (def_set, use_set) *)
let def_inst (inst : inst_ir) : OperandSet.t * OperandSet.t =
  let op_set op = match op with Imm _ -> OperandSet.empty | _ -> OperandSet.singleton op in
  let args_set args =
    List.fold_left
      (fun acc op -> match op with Imm _ -> acc | _ -> OperandSet.add op acc)
      OperandSet.empty args
  in
  match inst with
  | Binop (_, dst, lhs, rhs) ->(*目标操作数 dst 被定义，源操作数 lhs 和 rhs 被使用 。*)
      (OperandSet.singleton dst, OperandSet.union (op_set lhs) (op_set rhs))
  | Unop (_, dst, src) ->
      (OperandSet.singleton dst, op_set src)
  | Assign (dst, src) ->
      (OperandSet.singleton dst, op_set src)
  | Load (dst, src) ->
      (OperandSet.singleton dst, op_set src)
  | Store (dst, src) ->
      (OperandSet.empty, OperandSet.union (op_set dst) (op_set src))
  | Call (dst, _, args) ->
      let def = (match dst with Imm _ -> OperandSet.empty | _ -> OperandSet.singleton dst) in
      (def, args_set args)
  | TailCall (_, args) -> (OperandSet.empty, args_set args)
  | Ret (Some op) -> (OperandSet.empty, op_set op)
  | Ret None -> (OperandSet.empty, OperandSet.empty)
  | IfGoto (cond, _) -> (OperandSet.empty, op_set cond)
  | Goto _ | Label _ -> (OperandSet.empty, OperandSet.empty)

(* 与 def_inst 类似，但专门处理基本块的终结符指令  *)
let def_termin = function
  | TermRet (Some op) ->
      (OperandSet.empty, (match op with Imm _ -> OperandSet.empty | _ -> OperandSet.singleton op))
  | TermRet None -> (OperandSet.empty, OperandSet.empty)
  | TermIf (cond, _, _) ->
      (OperandSet.empty, (match cond with Imm _ -> OperandSet.empty | _ -> OperandSet.singleton cond))
  | TermGoto _ | TermSeq _ -> (OperandSet.empty, OperandSet.empty)


(* 聚合块内所有指令和终结符的 def/use 集合，计算整个基本块的 def 和 use 集合 。 *)
let def_block (blk : block_ir) : OperandSet.t * OperandSet.t =
  let defs, uses =
    List.fold_right
      (fun inst (defs, uses) ->
        let def, use = def_inst inst in
        let new_uses = OperandSet.union use uses in
        let new_defs = OperandSet.union def defs in
        (new_defs, new_uses))
      blk.insts
      (OperandSet.empty, OperandSet.empty)
  in
  let term_def, term_use = def_termin blk.terminator in
  let final_use = OperandSet.union term_use uses in
  let final_def = OperandSet.union defs term_def in
  (final_def, final_use)


  (* 活变量分析主算法  *)
  (* 经典的不动点迭代算法。 *)
  (* 活跃性信息是反向传播的，即从程序的末尾向开头流动。
  一个变量在某点是活跃的，取决于它是否在未来的路径上被使用。
  算法通过不断迭代，直到所有块的 l_in 和 l_out 集合不再变化，
  达到一个“不动点”为止。 *)
let liv_analy (blocks : block_ir list) (print_liveness : bool) : unit =
  let changed = ref true in
  List.iter
    (fun blk ->
      blk.l_in <- OperandSet.empty;
      blk.l_out <- OperandSet.empty)
    blocks;

  while !changed do
    changed := false;
    List.iter
      (fun blk ->
        let succ_l_in =
          List.fold_left
            (fun acc lbl ->
              match List.find_opt (fun b -> b.label = lbl) blocks with
              | Some b -> OperandSet.union acc b.l_in
              | None -> acc)
            OperandSet.empty blk.succs
        in
        let def, use = def_block blk in
        let new_in = OperandSet.union use (OperandSet.diff succ_l_in def) in

        if not (OperandSet.equal blk.l_in new_in)
           || not (OperandSet.equal blk.l_out succ_l_in)
        then changed := true;

        blk.l_in <- new_in;
        blk.l_out <- succ_l_in)
      blocks
  done;

  if print_liveness then pri_info blocks
