%{ open Ast %}

%token SEMICOLON
%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <string> IDENTIFIER
%token ASSIGN
%token RETURN
%token PLUS MINUS TIMES DIVIDE 
%token TIMES_ASSIGN DIVIDE_ASSIGN MOD_ASSIGN PLUS_ASSIGN MINUS_ASSIGN LSHIFT_ASSIGN RSHIFT_ASSIGN AND_ASSIGN XOR_ASSIGN OR_ASSIGN 
%token AUTO REGISTER STATIC EXTERN TYPEDEF
%token VOID CHAR SHORT INT LONG FLOAT DOUBLE SIGNED UNSIGNED
%token CONST VOLATILE
%token STRUCT UNION
%token SWITCH CASE ENUM DEFAULT IF ELSE
%token LBRACKET RBRACKET LBRACKET_SQUARE RBRACKET_SQUARE LPAREN RPAREN COMMA
COLON ELLIPSIS ASTERISK
%token WHILE DO FOR GOTO CONTINUE BREAK
%token QUESTION
%token EOF

%start func_decl
%type <Ast.tFuncDecl> func_decl

%%

statement_list:
  /* nothing */ { [] }
  | statement_list statement { $2::$1 } 

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
  IDENTIFIER assignment_operator expr { AsnExpr(Identifier($1), $2, $3) } 

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

storage_class_specifier:
        AUTO   { Auto }  
       | REGISTER { Register }
       | STATIC   { Static }
       | EXTERN  { Extern }
       | TYPEDEF { Typedef }

declaration_specifiers:
    type_specifier { DeclSpecTypeSpec($1) } 
  | type_specifier declaration_specifiers { DeclSpecTypeSpecInitList($1, $2) }

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

declaration:
  declaration_specifiers init_declarator_list SEMICOLON { Declaration($1, $2)}

declaration_list:
   /* Nothing */ { [] }
   | declaration_list declaration { $2 :: $1 }

compound_statement:
     LBRACKET declaration_list statement_list RBRACKET { ((List.rev $2),
     (List.rev $3)) }

func_params:
    declaration_specifiers declarator { FuncParamsDeclared($1, $2) }
  | declaration_specifiers { ParamDeclWithType($1) }


func_params_list:
   /* Nothing */ { [] }
   | func_params { [$1] }
   | func_params_list COMMA func_params { $3 :: $1 }

func_decl:
     declaration_specifiers declarator LPAREN func_params_list RPAREN compound_statement { {
             return_type = $1;
             name = $2;
             params = ($4);
             body = $6 }}
