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
        declarator assignment_operator assignment_expression { AsnExpr($1, $2, $3) }  

assignment_operator:
        ASSIGN { Asn }
       | TIMES_ASSIGN { MulAsn }
       | DIVIDE_ASSIGN { DivAsn }
       | MOD_ASSIGN { ModAsn }
       | PLUS_ASSIGN { AddAsn }
       | MINUS_ASSIGN { SubAsn }
       | LSHIFT_ASSIGN { LshAsn }
       | RSHIFT_ASSIGN { RshAsn }
       | AND_ASSIGN { AndAsn }
       | XOR_ASSIGN { XorAsn }
       | OR_ASSIGN { OrAsn }

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

declaration:
    declaration_specifiers SEMICOLON { Declaration($1) }
  | declaration_specifiers init_declarator_list SEMICOLON { DeclarationList($1, $2)}

declaration_specifiers:
    type_specifier { DeclSpecTypeSpec($1) } 
  | type_specifier declaration_specifiers { DeclSpecTypeSpecInitList($1, $2) }

type_specifier:
    VOID { Void }
  | CHAR { Char }
  | SHORT { Short }
  | INT { Int }
  | LONG { Long }
  | FLOAT { Float }
  | DOUBLE { Double }
  | SIGNED { Signed }
  | UNSIGNED { Unsigned }

init_declarator_list:
        init_declarator { InitDeclList([$1]) }  
  | init_declarator_list COMMA init_declarator { InitDeclList($3::[$1])}

init_declarator:
    declarator  { InitDeclarator($1) }
  | declarator ASSIGN assignment_expression { InitDeclaratorAsn($1, Asn, $3) }

declarator:
    direct_declarator { DirectDeclarator($1) }

direct_declarator:
    IDENTIFIER { Var(Identifier($1)) }
