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

(* open Toyc_compiler_lib

let () =
  let source_code =
    "int factorial(int n)\n\
     {\n\
    \    if (n <= 1)\n\
    \    {\n\
    \        return 1;\n\
    \    }\n\
    \    return n * factorial(n - 1);\n\
     }\n\n\
     int fibonacci(int n)\n\
     {\n\
    \    if (n <= 0)\n\
    \    {\n\
    \        return 0;\n\
    \    }\n\
    \    if (n == 1)\n\
    \    {\n\
    \        return 1;\n\
    \    }\n\
    \    return fibonacci(n - 1) + fibonacci(n - 2);\n\
     }\n\n\
     int gcd(int a, int b)\n\
     {\n\
    \    if (b == 0)\n\
    \    {\n\
    \        return a;\n\
    \    }\n\
    \    return gcd(b, a % b);\n\
     }\n\n\
     int is_prime(int n)\n\
     {\n\
    \    if (n <= 1)\n\
    \    {\n\
    \        return 0;\n\
    \    }\n\
    \    if (n <= 3)\n\
    \    {\n\
    \        return 1;\n\
    \    }\n\
    \    if (n % 2 == 0 || n % 3 == 0)\n\
    \    {\n\
    \        return 0;\n\
    \    }\n\n\
    \    int i = 5;\n\
    \    while (i * i <= n)\n\
    \    {\n\
    \        if (n % i == 0 || n % (i + 2) == 0)\n\
    \        {\n\
    \            return 0;\n\
    \        }\n\
    \        i = i + 6;\n\
    \    }\n\
    \    return 1;\n\
     }\n\n\
     int main()\n\
     {\n\
    \    int a = 1;\n\
    \    int b = 2;\n\
    \    int c = 3;\n\
    \    int d = 4;\n\n\
    \    int expr1 = ((a + b) * c + (d * a)) / ((b + c) % (a + -d + 2048) + 1) + (a * b \
     * c * (d - 2 - c));\n\n\
    \    int x = 0;\n\
    \    int y = 1;\n\
    \    int z = 2;\n\n\
    \    int expr2 = 0;\n\
    \    if ((x > y) && ((z + 1) == 1))\n\
    \    {\n\
    \        expr2 = 1;\n\
    \    }\n\n\
    \    int expr3 = 0;\n\
    \    if ((x < y) || ((z + 2) == 2))\n\
    \    {\n\
    \        expr3 = 1;\n\
    \    }\n\n\
    \    int expr4 = 0;\n\
    \    if (!((x > 0 && y < 0) || (z > 0 && x < 0)) && (y > 0 || x < 0))\n\
    \    {\n\
    \        expr4 = 1;\n\
    \    }\n\n\
    \    int n1 = 42;\n\
    \    int n2 = 56;\n\
    \    int n3 = 87;\n\n\
    \    int expr5 = factorial(gcd(n2, n3)) + fibonacci(n1 / 5);\n\n\
    \    int max_val = 0;\n\
    \    if (n1 > n2 && n1 > n3)\n\
    \    {\n\
    \        max_val = n1;\n\
    \    }\n\
    \    else if (n2 > n1 && n2 > n3)\n\
    \    {\n\
    \        max_val = n2;\n\
    \    }\n\
    \    else\n\
    \    {\n\
    \        max_val = n3;\n\
    \    }\n\n\
    \    int sum = 0;\n\
    \    int i = 1;\n\
    \    while (i <= 10)\n\
    \    {\n\
    \        if (i % 2 == 0)\n\
    \        {\n\
    \            sum = sum + i * i;\n\
    \        }\n\
    \        else if (i % 3 == 0)\n\
    \        {\n\
    \            sum = sum + i * i * i;\n\
    \        }\n\
    \        else\n\
    \        {\n\
    \            sum = sum + i;\n\
    \        }\n\
    \        i = i + 1;\n\
    \    }\n\n\
    \    int expr6 = 0;\n\
    \    i = 1;\n\
    \    while (i <= 5)\n\
    \    {\n\
    \        int j = 1;\n\
    \        int term = 1;\n\
    \        while (j <= i)\n\
    \        {\n\
    \            term = term * j;\n\
    \            j = j + 1;\n\
    \        }\n\
    \        expr6 = expr6 + term;\n\
    \        i = i + 1;\n\
    \    }\n\n\
    \    int expr7 = 0;\n\
    \    if (is_prime(n1))\n\
    \    {\n\
    \        if (is_prime(n2))\n\
    \        {\n\
    \            expr7 = n1 * n2;\n\
    \        }\n\
    \        else if (is_prime(n3))\n\
    \        {\n\
    \            expr7 = n1 * n3;\n\
    \        }\n\
    \        else\n\
    \        {\n\
    \            expr7 = n1;\n\
    \        }\n\
    \    }\n\
    \    else if (is_prime(n2))\n\
    \    {\n\
    \        if (is_prime(n3))\n\
    \        {\n\
    \            expr7 = n2 * n3;\n\
    \        }\n\
    \        else\n\
    \        {\n\
    \            expr7 = n2;\n\
    \        }\n\
    \    }\n\
    \    else if (is_prime(n3))\n\
    \    {\n\
    \        expr7 = n3;\n\
    \    }\n\
    \    else\n\
    \    {\n\
    \        expr7 = n1 + n2 + n3;\n\
    \    }\n\n\
    \    int expr8 = 0;\n\
    \    int num = 2345;\n\
    \    int bit_count = 0;\n\n\
    \    while (num > 0)\n\
    \    {\n\
    \        if (num % 2 == 1)\n\
    \        {\n\
    \            bit_count = bit_count + 1;\n\
    \        }\n\
    \        num = num / 2;\n\
    \    }\n\n\
    \    int final_result = expr1 + expr2 + expr3 + expr4 + expr5 + max_val + sum + \
     expr6 + expr7 + bit_count;\n\n\
    \    return final_result % 256;\n\
     }\n\n"
  in
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
    print_endline "Generated Assembly:";
    List.iter print_endline assembly
  with
  | Toyc_compiler_lib.Lexer.Error msg -> Printf.eprintf "Lexer Error: %s\n" msg
  | e ->
    Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string e);
    Printexc.print_backtrace stderr;
    exit 1
;; *)

open Toyc_compiler_lib

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
    let ir_code = Codegen.gen_program ast in
    let assembly = Codegen.gen_assembly ir_code in
    List.iter print_endline assembly
  with
  | Toyc_compiler_lib.Lexer.Error msg -> prerr_endline ("Lexer Error: " ^ msg)
  | e ->
    prerr_endline ("Unexpected error: " ^ Printexc.to_string e);
    Printexc.print_backtrace stderr;
    exit 1
;;
