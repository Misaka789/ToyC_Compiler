(* debug版本存档 *)

open Toyc_compiler_lib
(*
   (* 硬编码的文件名 *)
(* let input_file = "17_complex_expressions.tc"    (* 输入文件名 *)
let input_file = "18_many_variables.tc"
let input_file = "19_many_arguments.tc"
let input_file = "20_comprehensive.tc" *)
let input_file = "test.tc"
let output_file = "test.txt" (* 输出文件名 *)

let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let write_file filename content =
  let ch = open_out filename in
  output_string ch content;
  close_out ch

let () =
  let buffer = Buffer.create 1024 in
  let append_to_buffer s = Buffer.add_string buffer (s ^ "\n") in
  
  try
    (* 1. 读取输入文件 *)
    let source_code = read_file input_file in
    (* append_to_buffer ("Attempting to parse:\n---\n" ^ source_code ^ "\n---"); *)

    (* 2. 词法分析 & 语法分析 *)
    let lexbuf = Lexing.from_string source_code in
    let ast = Parser.program Lexer.token lexbuf in
    append_to_buffer "Success! AST generated:";
    append_to_buffer (Toyc_compiler_lib.Ast.string_of_program ast);

    (* 3. 生成IR *)
    let ir_code = Codegen.gen_program ast in
    append_to_buffer "======================================";
    append_to_buffer "Generated IR Code:";
    append_to_buffer "--------------------------------------";
    List.iter (fun instr -> append_to_buffer (Codegen.string_of_ir instr)) ir_code;
    append_to_buffer "======================================";

    (* 4. 生成汇编 *)
    let assembly = Codegen.gen_assembly ir_code in
    append_to_buffer "Generated Assembly:";
    List.iter append_to_buffer assembly;

    (* 5. 写入输出文件 *)
    write_file output_file (Buffer.contents buffer);
    print_endline ("Compilation completed. Results written to " ^ output_file)

  with
  | Sys_error msg -> 
      append_to_buffer ("File error: " ^ msg);
      write_file output_file (Buffer.contents buffer);
      exit 1
  | Toyc_compiler_lib.Lexer.Error msg ->
      append_to_buffer ("Lexer Error: " ^ msg);
      write_file output_file (Buffer.contents buffer);
      exit 1
  | e ->
      append_to_buffer ("Unexpected error: " ^ Printexc.to_string e);
      Printexc.print_backtrace stderr;
      write_file output_file (Buffer.contents buffer);
      exit 1
  ;; *)

let read_stdin_all () =
  let buf = Buffer.create 4096 in
  try
    while true do
      Buffer.add_string buf (input_line stdin);
      Buffer.add_char buf '\n'
    done;
    Buffer.contents buf
  with
  | End_of_file -> Buffer.contents buf
;;

let () =
  let source_code = read_stdin_all () in
  let lexbuf = Lexing.from_string source_code in
  try
    let ast = Parser.program Lexer.token lexbuf in
    (*调用优化函数*)
    let optimized_ast = Codegen.optimize_program ast in
    let ir_code = Codegen.gen_program optimized_ast in
    let assembly = Codegen.gen_assembly ir_code in
    List.iter print_endline assembly
  with
  | Toyc_compiler_lib.Lexer.Error msg -> prerr_endline ("Lexer Error: " ^ msg)
  | e ->
    prerr_endline ("Unexpected error: " ^ Printexc.to_string e);
    Printexc.print_backtrace stderr;
    exit 1
;;
