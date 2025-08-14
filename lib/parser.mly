
%{
  open Ast
%}



%token IF ELSE WHILE BREAK CONTINUE RETURN INT VOID


%token <int> NUMBER
%token <string> ID


%token PLUS MINUS TIMES DIVIDE MOD
%token ASSIGN EQ NEQ LT LE GT GE
%token AND OR NOT
%token LPAREN RPAREN LBRACE RBRACE SEMICOLON COMMA


%token EOF

%right ASSIGN                     /* a = b = c is parsed as a = (b = c) */
%left OR                          /* a || b || c is parsed as (a || b) || c */
%left AND
%nonassoc EQ NEQ                  /* a == b == c is a syntax error */
%nonassoc LT LE GT GE             /* a < b < c is a syntax error */
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NOT NEG                    /* A dummy token for unary minus/not precedence */

%start <Ast.program> program
//%start <Ast.expr> single_expr (*用于调试的入口 *)
%token Menhir_error

%%


program: (*程序的主入口点*)
  | funcs=nonempty_list(func_def) EOF { funcs }
single_expr :
  | e=expr EOF { e }
/* FuncDef -> ("int" | "void") ID "(" (Param ("," Param)*)? ")" Block */
// func_def:
//   | INT fname=ID LPAREN params=separated_list(COMMA, param) RPAREN body=block {
//       { fname=fname; params=params; body=body }
//     }
//   | error RBRACE {failwith "Syntax error in function definition, perhaps missing a '}'?"}
//   /* 这里我们现在只是返回int类型的值 */

func_def:
  | INT fname=ID LPAREN params=separated_list(COMMA, param) RPAREN body=block {
      { fname; params; rettyp = TInt; body }
    }
  | VOID fname=ID LPAREN params=separated_list(COMMA, param) RPAREN body=block {
      { fname; params; rettyp = TVoid; body }
    }
  | error RBRACE { failwith "Syntax error in function definition" }
/* Param -> "int" ID */
param:
  | INT name=ID { name }

/* Block -> "{" Stmt* "}" */
block:
  | LBRACE stmts=list(stmt) RBRACE { stmts }

/* Stmt -> ... */
stmt:
  | e=expr SEMICOLON { Expr(e) }
  | SEMICOLON { Expr(IntLiteral(0)) } (* 添加空语句支持 *)  (*Toyc包含空语句，即一个分号独占一行；*)
  | ID ASSIGN e=expr SEMICOLON { Assign($1, e) }
  | INT name=ID ASSIGN e=expr SEMICOLON { VarDecl(name, e) }
  | IF LPAREN cond=expr RPAREN then_stmt=stmt { If(cond, then_stmt, None) }
  | IF LPAREN cond=expr RPAREN then_stmt=stmt ELSE else_stmt=stmt { If(cond, then_stmt, Some(else_stmt)) }
  | WHILE LPAREN cond=expr RPAREN loop_body=stmt { While(cond, loop_body) }
  | BREAK SEMICOLON { Break }
  | CONTINUE SEMICOLON { Continue }
  // | RETURN e=expr SEMICOLON { Return(Some(e)) }
  | RETURN SEMICOLON { Return None }
  | RETURN e=expr SEMICOLON { Return (Some (e)) }
  | inner_block=block { Block(inner_block) } /* A block can also be a statement */
  | error SEMICOLON {Printf.eprintf "Syntax error in statement . Skipping to the next semicolon. \n"
                    failwith "Syntax error in statment."}
   /*在解析stmt 语句的是偶出错, 就一直忽略token直到下一个; 到来*/

expr:
  | LOrExpr { $1 }

/* LOrExpr -> LAndExpr | LOrExpr "||" LAndExpr */
LOrExpr:
  | LAndExpr { $1 }
  | LOrExpr OR LAndExpr { BinOp(Or, $1, $3) }

/* LAndExpr -> RelExpr | LAndExpr "&&" RelExpr */
LAndExpr:
  | RelExpr { $1 }
  | LAndExpr AND RelExpr { BinOp(And, $1, $3) }

/* RelExpr -> AddExpr | RelExpr ("<" | ">" | "<=" | ">=" | "==" | "!=") AddExpr */
RelExpr:
  | AddExpr { $1 }
  | RelExpr LT AddExpr  { BinOp(Lt,  $1, $3) }
  | RelExpr LE AddExpr  { BinOp(Le,  $1, $3) }
  | RelExpr GT AddExpr  { BinOp(Gt,  $1, $3) }
  | RelExpr GE AddExpr  { BinOp(Ge,  $1, $3) }
  | RelExpr EQ AddExpr  { BinOp(Eq,  $1, $3) }
  | RelExpr NEQ AddExpr { BinOp(Neq, $1, $3) }

/* AddExpr -> MulExpr | AddExpr ("+" | "-") MulExpr */
AddExpr:
  | MulExpr { $1 }
  | AddExpr PLUS MulExpr  { BinOp(Add, $1, $3) }
  | AddExpr MINUS MulExpr { BinOp(Sub, $1, $3) }

/* MulExpr -> UnaryExpr | MulExpr ("*" | "/" | "%") UnaryExpr */
MulExpr:
  | UnaryExpr { $1 }
  | MulExpr TIMES UnaryExpr  { BinOp(Mul, $1, $3) }
  | MulExpr DIVIDE UnaryExpr { BinOp(Div, $1, $3) }
  | MulExpr MOD UnaryExpr    { BinOp(Mod, $1, $3) }

/* UnaryExpr -> PrimaryExpr | ("+" | "-" | "!") UnaryExpr */ 
UnaryExpr:
  | PrimaryExpr { $1 } 
   | PLUS UnaryExpr  { $2 } /* 正数形式的可以直接优化掉*/
   | MINUS UnaryExpr %prec NEG { UnOp(Neg, $2) } /* Use %prec to give it correct precedence */
   | NOT UnaryExpr   %prec NEG { UnOp(Not, $2) }

/* PrimaryExpr -> ID | NUMBER | "(" Expr ")" | ID "(" (Expr ("," Expr)*)? ")" */
PrimaryExpr:
  | n=NUMBER { IntLiteral(n) }
  | name=ID { Id(name) }
  | LPAREN e=expr RPAREN { e }  (*去多余括号*)
  | name=ID LPAREN args=separated_list(COMMA, expr) RPAREN { Call(name, args) }
  | error {failwith "Syntax error in expression."}

  (*eg:  foo(1, x + 2, bar());
    ast: Call(Id("foo"), [IntLiteral(1); BinOp(Add, Id("x"), IntLiteral(2)); Call(Id("bar"), [])])*)