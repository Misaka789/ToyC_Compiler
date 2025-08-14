
(* lib/linearReg.ml *)
(* 线性扫描寄存器分配 *)
(* 将程序中无限的虚拟寄存器（或变量）有效地映射到 CPU 有限的物理寄存器上。 *)
(* 核心功能是接收一个函数的中间表示（IR），分析其中变量活性区间，然后根据这些区间为变量分配物理寄存器。当物理寄存器不足时，它会将某些变量“溢出”（Spill）到内存中。 *)

open Ir

module O_key = struct
  type t = operand

  let equal a b =
    match (a, b) with
    | Var x, Var y -> String.equal x y(*一个程序变量或虚拟寄存器 *)
    | Reg x, Reg y -> String.equal x y(*一个物理寄存器 *)
    | Imm x, Imm y -> x = y
    | _, _ -> false
(* 使用哈希表，如果不使用哈希表，用关联列表，即 (key * value) list，性能下降 *)
  let hash = function
    | Var x -> Hashtbl.hash ("var", x)
    | Reg x -> Hashtbl.hash ("reg", x)
    | Imm i -> Hashtbl.hash ("imm", i)
end

module O_hash = Hashtbl.Make (O_key)


let spi_map : (operand, int) Hashtbl.t = Hashtbl.create 32
let stack_count = ref 0

let k_registers = 23

(* 物理寄存器列表 *)
let phy_reg =
  [|
    "a0";
    "a1";
    "a2";
    "a3";
    "a4";
    "a5";
    "a6";
    "a7";
    "t0";
    "t1";
    "t2";
    "t3";
    "s1";
    "s2";
    "s3";
    "s4";
    "s5";
    "s6";
    "s7";
    "s8";
    "s9";
    "s10";
    "s11";
  |]

  (* 活性区间的记录类型 *)
  type inter = {
  name : operand;
  mutable start : int;
  mutable end_ : int;
  is_param : bool;
}

(* 构建区间 *)
(* 分析一个函数（类型为 ir_func_o），并为其中的每个变量（operand）构建存活区间 inter 列表。 *)
let b_inter (f : ir_func_o) : inter list =
  let reg_inters = Hashtbl.create 128 in
  let block_pos = Hashtbl.create 64 in
  let counter = ref 0 in

  List.iter
    (fun block ->
      incr counter;
      Hashtbl.add block_pos block.label !counter)
    f.blocks;

  let par_oper op =
    match op with Var name -> List.mem name f.args | _ -> false
  in

  let add_live v pos =
    let op = v in
    match Hashtbl.find_opt reg_inters op with
    | Some i ->
        i.start <- min i.start pos;
        i.end_ <- max i.end_ pos
    | None ->
        let i =
          { name = op; start = pos; end_ = pos; is_param = par_oper op }
        in
        Hashtbl.add reg_inters op i
  in

  List.iter
    (fun block ->
      let pos = Hashtbl.find block_pos block.label in
      OperandSet.iter (fun v -> add_live v pos) block.l_in;
      OperandSet.iter (fun v -> add_live v pos) block.l_out)
    f.blocks;

  Hashtbl.fold (fun _ itv acc -> itv :: acc) reg_inters []

  (* 打印函数 *)
(* let print_reg reg_map : unit =
  Printf.printf "=== Register Allocation Result ===\n";
  O_hash.iter
    (fun op reg ->
      let name =
        match op with
        | Reg r -> Printf.sprintf "Reg %s" r
        | Var v -> Printf.sprintf "Var %s" v
        | Imm i -> Printf.sprintf "Imm %d" i
      in
      Printf.printf "%-10s -> %s\n" name reg)
    reg_map *)


(* 实现线性扫描算法的主函数 *)
(* 输入inters: 由 b_inter 生成的存活区间列表。 *)
let lin_alloca (inters : inter list) (p_alloca : bool) =
  let inters = List.sort (fun a b -> compare a.start b.start) inters in
  let active : inter list ref = ref [] in

  let reg_map = O_hash.create 32 in
  let alloc_map = O_hash.create 512 in

  (* 清理过期区间 *)
  let exp_inter current =
    active :=
      List.filter
        (fun itv ->
          if itv.end_ >= current.start then true
          else (
            O_hash.remove reg_map itv.name;
            false))
        !active
  in

  List.iter
    (fun itv ->
      exp_inter itv;
      (* 分配寄存器 *)
      if itv.is_param then (
        O_hash.add reg_map itv.name "__SPILL__";
        O_hash.replace alloc_map itv.name "__SPILL__")
      (* 寄存器用尽 *)
      else if List.length !active = k_registers then (
        (if
           p_alloca
         then
           let name =
             match itv.name with Reg r | Var r -> r | Imm _ -> "imm"
           in
           Printf.printf "Spill: %s\n" name);
        O_hash.add reg_map itv.name "__SPILL__";
        O_hash.replace alloc_map itv.name "__SPILL__")
      (* 成功分配 *)
      else
        let used_regs =
          O_hash.fold (fun _ r acc -> r :: acc) reg_map []
        in
        let avail =
          List.find
            (fun r -> not (List.mem r used_regs))
            (Array.to_list phy_reg)
        in
        O_hash.add reg_map itv.name avail;
        O_hash.replace alloc_map itv.name avail;
        active := itv :: !active)
    inters;
  if p_alloca then print_reg alloc_map;
  alloc_map
