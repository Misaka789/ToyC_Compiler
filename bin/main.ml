(* (* main.ml
let () =
  let input_file = Sys.argv.(1) in
  let output_file = Sys.argv.(2) in
  let in_channel = open_in input_file in
  let lexbuf = Lexing.from_channel in_channel in
  let ast = Parser.program Lexer.token lexbuf in
  let asm_code = Codegen.generate_code ast in
  let out_channel = open_out output_file in
  output_string out_channel asm_code;
  close_in in_channel;
  close_out out_channel
;;
*)

open Toyc_compiler_lib
open Ast

let () =
  let test_case = "int main() { int x = 10; return x + 32; }" in
  print_endline "======================================";
  print_endline "Input source code:";
  print_endline test_case;
  print_endline "======================================";
  (* 2. 从字符串创建词法分析缓冲区 *)
  (* Lexing.from_string 是关键，它让我们可以不依赖文件 *)
  let lexbuf = Lexing.from_string test_case in
  try
    (* 3. 运行解析器并获取 AST *)
    (* 这和从文件读取的调用方式完全一样 *)
    let ast = Parser.program Lexer.token lexbuf in
    (* 4. 打印出生成的 AST *)
    print_endline "Parsing successful! Generated AST:";
    print_endline "--------------------------------------";
    print_endline (string_of_program ast);
    print_endline "======================================"
  with
  | Lexer.Error msg -> Printf.eprintf "Lexer error: %s\n" msg
  | Parser.Error ->
    let pos = lexbuf.Lexing.lex_curr_p in
    Printf.eprintf
      "Parser error at line %d, character %d\n"
      pos.Lexing.pos_lnum
      (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
;;
*)

(* bin/main.ml - Debugging version *)


open Toyc_compiler_lib
(*
let () =
  let source_code = 
    "int main() {
    int x = 5;
    if (x > 0) {
        return 1;
    }
    return 0;
}
" in
  Printf.printf "Attempting to parse:\n---\n%s\n---\n" source_code;
  let lexbuf = Lexing.from_string source_code in
  try
    (* 用带命名空间的模块名 *)
    (*生成ast*)
    let ast = Parser.program Lexer.token lexbuf in
    print_endline "Success! AST generated:";
    print_endline (Toyc_compiler_lib.Ast.string_of_program ast);
    (*ignore(Codegen.gen_program ast)*)
    (*生成IR*)
     let ir_code = Codegen.gen_program ast in
    print_endline "======================================";
    print_endline "Generated IR Code:";
    print_endline "--------------------------------------";
    (* 使用 Codegen.string_of_ir 将每条 IR 指令转换为字符串后再打印 *)
    List.iter (fun instr -> print_endline (Codegen.string_of_ir instr)) ir_code;
    print_endline "======================================";
    (* 生成汇编 *)  
    let assembly = Codegen.gen_assembly ir_code in  
    print_endline "Generated Assembly:" ;
    List.iter print_endline assembly  
  with
  | Toyc_compiler_lib.Lexer.Error msg -> Printf.eprintf "Lexer Error: %s\n" msg
  | e ->
    Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string e);
    Printexc.print_backtrace stderr;
    exit 1
;;

*)

let read_stdin_all () =
  let buf = Buffer.create 4096 in
  try
    while true do
      Buffer.add_string buf (input_line stdin);
      Buffer.add_char buf '\n'
    done;
    Buffer.contents buf
  with End_of_file ->
    Buffer.contents buf

let () =
  let source_code = read_stdin_all () in
  let lexbuf = Lexing.from_string source_code in
  try
    let ast = Parser.program Lexer.token lexbuf in
    let ir_code = Codegen.gen_program ast in
    let assembly = Codegen.gen_assembly ir_code in
    List.iter print_endline assembly
  with
  | Toyc_compiler_lib.Lexer.Error msg ->
      prerr_endline ("Lexer Error: " ^ msg)
  | e ->
      prerr_endline ("Unexpected error: " ^ Printexc.to_string e);
      Printexc.print_backtrace stderr;
      exit 1




