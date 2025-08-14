(* lib/ir.ml *)
(* 定义了IR的数据结构。 *)



type operand =
  | Reg of string 
  | Imm of int 
  | Var of string

(* 操作数集合，用于数据流分析。 *)
module OperandSet = Set.Make (struct
  type t = operand
  let compare = compare
end)


type inst_ir =
  | Binop of string * operand * operand * operand 
  | Unop of string * operand * operand 
  | Load of operand * operand 
  | Store of operand * operand 
  | Goto of string 
  | IfGoto of operand * string 
  | Label of string 
  | Call of operand * string * operand list
  | TailCall of string * operand list 
  | Ret of operand option 
  | Assign of operand * operand 

  type term_ir =
  | TermGoto of string
  | TermIf of operand * string * string
  | TermRet of operand option
  | TermSeq of string

  type block_ir = {
  label : string;
  mutable insts : inst_ir list;
  mutable terminator : term_ir;
  mutable preds : string list;
  mutable succs : string list;
  mutable l_in : OperandSet.t;
  mutable l_out : OperandSet.t;
}

(* 被组织成基本块之前 *)
type func_ir = { name : string; args : string list; body : inst_ir list }

(* 被组织成基本块（即控制流图）之后 *)
type ir_func_o = { name : string; args : string list; blocks : block_ir list }
type ir_program = Ir_funcs of func_r list | Ir_funcs_o of ir_func_o list
