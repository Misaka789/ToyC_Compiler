type token =
  | ID of (
# 6 "lib/parser.mly"
        string
# 6 "lib/parser.ml"
)
  | NUMBER of (
# 7 "lib/parser.mly"
        int
# 11 "lib/parser.ml"
)
  | INT
  | VOID
  | IF
  | IFX
  | ELSE
  | WHILE
  | BREAK
  | CONTINUE
  | RETURN
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | MOD
  | EQ
  | NEQ
  | LE
  | GE
  | LT
  | GT
  | LAND
  | LOR
  | NOT
  | ASSIGN
  | SEMI
  | COMMA
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | EOF

open Parsing
let _ = parse_error;;
# 2 "lib/parser.mly"
open Ast
# 49 "lib/parser.ml"
let yytransl_const = [|
  259 (* INT *);
  260 (* VOID *);
  261 (* IF *);
  262 (* IFX *);
  263 (* ELSE *);
  264 (* WHILE *);
  265 (* BREAK *);
  266 (* CONTINUE *);
  267 (* RETURN *);
  268 (* PLUS *);
  269 (* MINUS *);
  270 (* TIMES *);
  271 (* DIV *);
  272 (* MOD *);
  273 (* EQ *);
  274 (* NEQ *);
  275 (* LE *);
  276 (* GE *);
  277 (* LT *);
  278 (* GT *);
  279 (* LAND *);
  280 (* LOR *);
  281 (* NOT *);
  282 (* ASSIGN *);
  283 (* SEMI *);
  284 (* COMMA *);
  285 (* LPAREN *);
  286 (* RPAREN *);
  287 (* LBRACE *);
  288 (* RBRACE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* ID *);
  258 (* NUMBER *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\004\000\004\000\005\000\005\000\
\007\000\007\000\008\000\006\000\009\000\009\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\012\000\012\000\012\000\012\000\
\013\000\013\000\014\000\014\000\000\000"

let yylen = "\002\000\
\002\000\001\000\002\000\006\000\001\000\001\000\000\000\001\000\
\001\000\003\000\002\000\003\000\000\000\002\000\001\000\001\000\
\002\000\004\000\003\000\005\000\005\000\007\000\005\000\002\000\
\002\000\002\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\002\000\002\000\002\000\001\000\001\000\001\000\003\000\004\000\
\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\005\000\006\000\053\000\000\000\002\000\000\000\
\001\000\003\000\000\000\000\000\000\000\000\000\000\000\009\000\
\011\000\000\000\000\000\013\000\004\000\010\000\000\000\000\000\
\046\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\016\000\000\000\012\000\015\000\014\000\000\000\
\044\000\000\000\000\000\000\000\000\000\000\000\024\000\025\000\
\000\000\026\000\000\000\043\000\042\000\041\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\017\000\000\000\000\000\000\000\
\000\000\000\000\019\000\000\000\000\000\027\000\047\000\000\000\
\000\000\028\000\029\000\030\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\018\000\048\000\000\000\000\000\
\000\000\000\000\000\000\020\000\000\000\023\000\000\000\022\000"

let yydgoto = "\002\000\
\005\000\006\000\007\000\008\000\014\000\038\000\015\000\016\000\
\023\000\039\000\040\000\041\000\072\000\073\000"

let yysindex = "\255\255\
\129\255\000\000\000\000\000\000\000\000\001\000\000\000\007\255\
\000\000\000\000\242\254\008\255\023\255\019\255\075\255\000\000\
\000\000\042\255\008\255\000\000\000\000\000\000\045\255\055\255\
\000\000\074\255\054\255\076\255\080\255\086\255\114\255\039\255\
\039\255\039\255\000\000\039\255\000\000\000\000\000\000\250\255\
\000\000\039\255\039\255\131\255\039\255\039\255\000\000\000\000\
\094\255\000\000\010\000\000\000\000\000\000\000\151\255\039\255\
\039\255\039\255\039\255\039\255\039\255\039\255\039\255\039\255\
\039\255\039\255\039\255\039\255\000\000\026\000\074\000\100\255\
\107\255\039\255\000\000\170\255\189\255\000\000\000\000\028\255\
\028\255\000\000\000\000\000\000\047\255\047\255\047\255\047\255\
\047\255\047\255\079\255\087\000\000\000\000\000\039\255\042\000\
\109\255\109\255\074\000\000\000\130\255\000\000\109\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\112\255\000\000\000\000\146\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\058\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\147\255\000\000\000\000\000\000\000\000\000\000\
\132\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\037\255\000\000\
\148\255\000\000\000\000\000\000\000\000\000\000\000\000\203\255\
\222\255\000\000\000\000\000\000\251\254\101\255\088\000\096\000\
\104\000\112\000\120\000\238\254\000\000\000\000\000\000\000\000\
\000\000\000\000\041\255\000\000\077\255\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\155\000\000\000\000\000\161\000\000\000\176\000\
\000\000\179\255\227\255\000\000\000\000\000\000"

let yytablesize = 406
let yytable = "\001\000\
\009\000\051\000\052\000\053\000\054\000\040\000\055\000\011\000\
\040\000\040\000\013\000\040\000\070\000\071\000\012\000\076\000\
\077\000\037\000\037\000\101\000\102\000\037\000\037\000\017\000\
\037\000\104\000\080\000\081\000\082\000\083\000\084\000\085\000\
\086\000\087\000\088\000\089\000\090\000\091\000\092\000\049\000\
\025\000\058\000\059\000\060\000\096\000\024\000\025\000\026\000\
\018\000\027\000\032\000\033\000\028\000\029\000\030\000\031\000\
\032\000\033\000\056\000\057\000\058\000\059\000\060\000\034\000\
\051\000\099\000\051\000\036\000\052\000\034\000\052\000\035\000\
\020\000\036\000\044\000\020\000\037\000\021\000\021\000\021\000\
\042\000\021\000\045\000\043\000\021\000\021\000\021\000\021\000\
\021\000\021\000\056\000\057\000\058\000\059\000\060\000\061\000\
\062\000\063\000\064\000\065\000\066\000\021\000\019\000\021\000\
\046\000\021\000\047\000\021\000\021\000\024\000\025\000\026\000\
\048\000\027\000\049\000\025\000\028\000\029\000\030\000\031\000\
\032\000\033\000\043\000\038\000\038\000\032\000\033\000\038\000\
\038\000\094\000\038\000\003\000\004\000\034\000\095\000\035\000\
\103\000\036\000\034\000\020\000\050\000\007\000\036\000\045\000\
\045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
\045\000\045\000\045\000\045\000\074\000\075\000\045\000\045\000\
\010\000\045\000\056\000\057\000\058\000\059\000\060\000\061\000\
\062\000\063\000\064\000\065\000\066\000\067\000\068\000\008\000\
\049\000\050\000\021\000\000\000\079\000\056\000\057\000\058\000\
\059\000\060\000\061\000\062\000\063\000\064\000\065\000\066\000\
\067\000\068\000\022\000\000\000\000\000\000\000\000\000\097\000\
\056\000\057\000\058\000\059\000\060\000\061\000\062\000\063\000\
\064\000\065\000\066\000\067\000\068\000\000\000\031\000\031\000\
\000\000\000\000\098\000\031\000\031\000\031\000\031\000\031\000\
\031\000\031\000\031\000\000\000\000\000\031\000\031\000\000\000\
\031\000\032\000\032\000\000\000\000\000\000\000\032\000\032\000\
\032\000\032\000\032\000\032\000\032\000\032\000\000\000\000\000\
\032\000\032\000\000\000\032\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\003\000\004\000\056\000\057\000\058\000\
\059\000\060\000\061\000\062\000\063\000\064\000\065\000\066\000\
\067\000\068\000\000\000\000\000\069\000\056\000\057\000\058\000\
\059\000\060\000\061\000\062\000\063\000\064\000\065\000\066\000\
\067\000\068\000\000\000\000\000\078\000\056\000\057\000\058\000\
\059\000\060\000\061\000\062\000\063\000\064\000\065\000\066\000\
\067\000\068\000\000\000\000\000\093\000\056\000\057\000\058\000\
\059\000\060\000\061\000\062\000\063\000\064\000\065\000\066\000\
\067\000\068\000\000\000\000\000\100\000\045\000\045\000\045\000\
\045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
\045\000\045\000\000\000\000\000\045\000\056\000\057\000\058\000\
\059\000\060\000\061\000\062\000\063\000\064\000\065\000\066\000\
\067\000\068\000\056\000\057\000\058\000\059\000\060\000\061\000\
\062\000\063\000\064\000\065\000\066\000\067\000\033\000\033\000\
\000\000\000\000\033\000\033\000\000\000\033\000\034\000\034\000\
\000\000\000\000\034\000\034\000\000\000\034\000\036\000\036\000\
\000\000\000\000\036\000\036\000\000\000\036\000\035\000\035\000\
\000\000\000\000\035\000\035\000\000\000\035\000\039\000\039\000\
\000\000\000\000\039\000\039\000\000\000\039\000"

let yycheck = "\001\000\
\000\000\031\000\032\000\033\000\034\000\024\001\036\000\001\001\
\027\001\028\001\003\001\030\001\042\000\043\000\029\001\045\000\
\046\000\023\001\024\001\097\000\098\000\027\001\028\001\001\001\
\030\001\103\000\056\000\057\000\058\000\059\000\060\000\061\000\
\062\000\063\000\064\000\065\000\066\000\067\000\068\000\001\001\
\002\001\014\001\015\001\016\001\074\000\001\001\002\001\003\001\
\030\001\005\001\012\001\013\001\008\001\009\001\010\001\011\001\
\012\001\013\001\012\001\013\001\014\001\015\001\016\001\025\001\
\028\001\095\000\030\001\029\001\028\001\025\001\030\001\027\001\
\031\001\029\001\001\001\031\001\032\001\001\001\002\001\003\001\
\026\001\005\001\029\001\029\001\008\001\009\001\010\001\011\001\
\012\001\013\001\012\001\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\025\001\028\001\027\001\
\029\001\029\001\027\001\031\001\032\001\001\001\002\001\003\001\
\027\001\005\001\001\001\002\001\008\001\009\001\010\001\011\001\
\012\001\013\001\029\001\023\001\024\001\012\001\013\001\027\001\
\028\001\030\001\030\001\003\001\004\001\025\001\028\001\027\001\
\007\001\029\001\025\001\031\001\027\001\030\001\029\001\012\001\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\026\001\027\001\027\001\028\001\
\006\000\030\001\012\001\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\030\001\
\030\001\030\001\018\000\255\255\030\001\012\001\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\019\000\255\255\255\255\255\255\255\255\030\001\
\012\001\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\255\255\012\001\013\001\
\255\255\255\255\030\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\255\255\255\255\027\001\028\001\255\255\
\030\001\012\001\013\001\255\255\255\255\255\255\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\255\255\255\255\
\027\001\028\001\255\255\030\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\003\001\004\001\012\001\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\255\255\255\255\027\001\012\001\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\255\255\255\255\027\001\012\001\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\255\255\255\255\027\001\012\001\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\255\255\255\255\027\001\012\001\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\255\255\255\255\027\001\012\001\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\012\001\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\023\001\024\001\
\255\255\255\255\027\001\028\001\255\255\030\001\023\001\024\001\
\255\255\255\255\027\001\028\001\255\255\030\001\023\001\024\001\
\255\255\255\255\027\001\028\001\255\255\030\001\023\001\024\001\
\255\255\255\255\027\001\028\001\255\255\030\001\023\001\024\001\
\255\255\255\255\027\001\028\001\255\255\030\001"

let yynames_const = "\
  INT\000\
  VOID\000\
  IF\000\
  IFX\000\
  ELSE\000\
  WHILE\000\
  BREAK\000\
  CONTINUE\000\
  RETURN\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  MOD\000\
  EQ\000\
  NEQ\000\
  LE\000\
  GE\000\
  LT\000\
  GT\000\
  LAND\000\
  LOR\000\
  NOT\000\
  ASSIGN\000\
  SEMI\000\
  COMMA\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  EOF\000\
  "

let yynames_block = "\
  ID\000\
  NUMBER\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'func_def_list) in
    Obj.repr(
# 34 "lib/parser.mly"
                        ( _1 )
# 313 "lib/parser.ml"
               : Ast.comp_unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'func_def) in
    Obj.repr(
# 37 "lib/parser.mly"
                              ( [_1] )
# 320 "lib/parser.ml"
               : 'func_def_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'func_def_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'func_def) in
    Obj.repr(
# 38 "lib/parser.mly"
                              ( _1 @ [_2] )
# 328 "lib/parser.ml"
               : 'func_def_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'param_list_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 42 "lib/parser.mly"
      (
        {
          ret_type = _1;
          func_name = _2;
          params = _4;
          body = _6;
        }
      )
# 345 "lib/parser.ml"
               : 'func_def))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "lib/parser.mly"
          ( TInt )
# 351 "lib/parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "lib/parser.mly"
          ( TVoid )
# 357 "lib/parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 56 "lib/parser.mly"
     ( [] )
# 363 "lib/parser.ml"
               : 'param_list_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'param_list) in
    Obj.repr(
# 57 "lib/parser.mly"
                  ( _1 )
# 370 "lib/parser.ml"
               : 'param_list_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'param) in
    Obj.repr(
# 60 "lib/parser.mly"
                            ( [_1] )
# 377 "lib/parser.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'param_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'param) in
    Obj.repr(
# 61 "lib/parser.mly"
                            ( _1 @ [_3] )
# 385 "lib/parser.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 64 "lib/parser.mly"
             ( _2 )
# 392 "lib/parser.ml"
               : 'param))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 66 "lib/parser.mly"
                              ( _2 )
# 399 "lib/parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    Obj.repr(
# 69 "lib/parser.mly"
     ( [] )
# 405 "lib/parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 70 "lib/parser.mly"
                   ( _1 @ [_2] )
# 413 "lib/parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 73 "lib/parser.mly"
            ( Block _1 )
# 420 "lib/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 74 "lib/parser.mly"
            ( Empty )
# 426 "lib/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 75 "lib/parser.mly"
                ( ExprStmt _1 )
# 433 "lib/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 76 "lib/parser.mly"
                          ( Assign (_1, _3) )
# 441 "lib/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 77 "lib/parser.mly"
                  ( Decl (_2, None) )
# 448 "lib/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 78 "lib/parser.mly"
                              ( Decl (_2, Some _4) )
# 456 "lib/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 80 "lib/parser.mly"
      ( If (_3, _5, None) )
# 464 "lib/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 82 "lib/parser.mly"
      ( If (_3, _5, Some _7) )
# 473 "lib/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 84 "lib/parser.mly"
      ( While (_3, _5) )
# 481 "lib/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "lib/parser.mly"
                 ( Break )
# 487 "lib/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "lib/parser.mly"
                    ( Continue )
# 493 "lib/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "lib/parser.mly"
                  ( Return None )
# 499 "lib/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 88 "lib/parser.mly"
                       ( Return (Some _2) )
# 506 "lib/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 91 "lib/parser.mly"
                      ( Binop (Mul, _1, _3) )
# 514 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 92 "lib/parser.mly"
                      ( Binop (Div, _1, _3) )
# 522 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 93 "lib/parser.mly"
                      ( Binop (Mod, _1, _3))
# 530 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 94 "lib/parser.mly"
                      ( Binop (Add, _1, _3) )
# 538 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 95 "lib/parser.mly"
                      ( Binop (Sub, _1, _3) )
# 546 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 96 "lib/parser.mly"
                   ( Binop (Leq, _1, _3) )
# 554 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 97 "lib/parser.mly"
                   ( Binop (Geq, _1, _3) )
# 562 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 98 "lib/parser.mly"
                   ( Binop (Greater, _1, _3) )
# 570 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 99 "lib/parser.mly"
                   ( Binop (Less, _1, _3) )
# 578 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 100 "lib/parser.mly"
                   ( Binop (Eq, _1, _3) )
# 586 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 101 "lib/parser.mly"
                    ( Binop (Neq, _1, _3) )
# 594 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 102 "lib/parser.mly"
                     ( Binop (Land, _1, _3) )
# 602 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 103 "lib/parser.mly"
                    ( Binop (Lor, _1, _3) )
# 610 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "lib/parser.mly"
               ( Unop (Not, _2) )
# 617 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 105 "lib/parser.mly"
                 ( Unop (Minus, _2))
# 624 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 106 "lib/parser.mly"
                ( Unop (Plus, _2))
# 631 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'primary) in
    Obj.repr(
# 107 "lib/parser.mly"
              ( _1 )
# 638 "lib/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 110 "lib/parser.mly"
         ( ID _1 )
# 645 "lib/parser.ml"
               : 'primary))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 111 "lib/parser.mly"
             ( Number _1 )
# 652 "lib/parser.ml"
               : 'primary))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 112 "lib/parser.mly"
                         ( _2 )
# 659 "lib/parser.ml"
               : 'primary))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'arg_list_opt) in
    Obj.repr(
# 113 "lib/parser.mly"
                                    ( Call (_1, _3) )
# 667 "lib/parser.ml"
               : 'primary))
; (fun __caml_parser_env ->
    Obj.repr(
# 116 "lib/parser.mly"
     ( [] )
# 673 "lib/parser.ml"
               : 'arg_list_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg_list) in
    Obj.repr(
# 117 "lib/parser.mly"
                  ( _1 )
# 680 "lib/parser.ml"
               : 'arg_list_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "lib/parser.mly"
                        ( [_1] )
# 687 "lib/parser.ml"
               : 'arg_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arg_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 121 "lib/parser.mly"
                        ( _1 @ [_3] )
# 695 "lib/parser.ml"
               : 'arg_list))
(* Entry comp_unit *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let comp_unit (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.comp_unit)
;;
