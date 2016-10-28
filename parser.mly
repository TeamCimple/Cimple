%{ open Ast %}

%token ASSIGN
%token RETURN
%token PLUS MINUS TIMES TIMES_ASSIGN DIVIDE DIVIDE_ASSIGN MOD_ASSIGN PLUS_ASSIGN
MINUS_ASSIGN LSHIFT_ASSIGN RSHIFT_ASSIGN AND_ASSIGN XOR_ASSIGN OR_ASSIGN EOF
%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
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
%token <string> IDENTIFIER

%start statement
%type <Ast.statement> statement

%%

statement_list:
  /* nothing */ { [] }
  | statement_list statement { $2 :: $1 }      

statement:
  expr_opt SEMICOLON { Expr $1 }
  | RETURN SEMICOLON { Return Noexpr }

expr_opt:
  /* Nothing */ {Noexpr}
  | expr          { $1 }

expr:
  add_expr  { $1 }
 | assignment_expression { $1 }

assignment_expression:
  IDENTIFIER ASSIGN expr { AsnOp($1, Asn, $3) }
 | IDENTIFIER TIMES_ASSIGN expr { AsnOp($1, MulAsn, $3) }
 | IDENTIFIER DIVIDE_ASSIGN expr { AsnOp($1, DivAsn, $3) }
 | IDENTIFIER MOD_ASSIGN expr { AsnOp($1, ModAsn, $3) }
 | IDENTIFIER PLUS_ASSIGN expr { AsnOp($1, AddAsn, $3) }
 | IDENTIFIER MINUS_ASSIGN expr { AsnOp($1, SubAsn, $3) }
 | IDENTIFIER LSHIFT_ASSIGN expr { AsnOp($1, LshAsn, $3) }
 | IDENTIFIER RSHIFT_ASSIGN expr { AsnOp($1, RshAsn, $3) }
 | IDENTIFIER AND_ASSIGN expr { AsnOp($1, AndAsn, $3) } 
 | IDENTIFIER XOR_ASSIGN expr { AsnOp($1, XorAsn, $3) }
 | IDENTIFIER OR_ASSIGN expr { AsnOp($1, OrAsn, $3) }

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
  | FLOAT_LITERAL            { Float($1) }
  | INT_LITERAL               { Literal($1) }
