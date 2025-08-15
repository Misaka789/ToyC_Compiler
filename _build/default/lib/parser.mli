type token =
  | ID of (
# 6 "lib/parser.mly"
        string
# 6 "lib/parser.mli"
)
  | NUMBER of (
# 7 "lib/parser.mly"
        int
# 11 "lib/parser.mli"
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

val comp_unit :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.comp_unit
