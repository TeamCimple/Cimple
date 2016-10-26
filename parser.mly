%{ open Ast %}

%token ASSIGN
%token RETURN
%token PLUS MINUS TIMES DIVIDE EOF
%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <char> CHAR_LITERAL
%token <string> STRING_LITERAL
%token <string> ID
%token SEMICOLON
%token AUTO REGISTER STATIC EXTERN TYPEDEF
%token VOID CHAR SHORT INT LONG FLOAT DOUBLE SIGNED UNSIGNED
%token CONST VOLATILE
%token STRUCT UNION
%token SWITCH CASE ENUM DEFAULT IF ELSE
%token LBRACKET RBRACKET LBRACKET_SQUARE RBRACKET_SQUARE LPAREN RPAREN COMMA
COLON ELLIPSIS ASTERISK
%token WHILE DO FOR GOTO CONTINUE BREAK
%token QUESTION

%start statement
%type <Ast.statement> statement

%%

statement_list:
  /* nothing */ { [] }
  | statement_list statement { $2 :: $1 }      

statement:
  expr_opt SEMICOLON { Expr $1 }
  | LBRACKET statement_list RBRACKET { Block(List.rev($2)) }
  | RETURN SEMICOLON { Return Noexpr }

expr_opt:
  /* Nothing */ {Noexpr}
  | expr          { $1 }

expr:
  add_expr  { $1 } 

add_expr:
  add_expr PLUS mult_expr { Binop($1, Add, $3) }
  | add_expr MINUS mult_expr { Binop($1, Sub, $3) }
  | mult_expr  { $1 }

mult_expr:
    mult_expr TIMES primary_expr { Binop($1, Mul, $3) }
  | mult_expr DIVIDE primary_expr { Binop($1, Div, $3) }
  | primary_expr             { $1 }

primary_expr:
  LPAREN expr RPAREN         { $2 }
  | const                    { $1 }
  | ID                        { Id($1) }

const:
  INT_LITERAL            { Literal($1) }
  | FLOAT_LITERAL        { Float($1) }
  | CHAR_LITERAL         { Char($1) }
  | STRING_LITERAL       { String($1) } 
