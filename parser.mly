%{ open Ast %}

%token SEMICOLON
%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <string> STRING_LITERAL
%token <string> IDENTIFIER
%token <string> STRUCT_IDENTIFIER
%token ASSIGN
%token RETURN
%token PLUS MINUS TIMES DIVIDE MOD
%token AND OR BITWISE_AND BITWISE_OR XOR NOT LSHIFT RSHIFT
%token EQUALS NOT_EQUALS LESS_THAN LESS_THAN_EQUALS GREATER_THAN GREATER_THAN_EQUALS
%token TIMES_ASSIGN DIVIDE_ASSIGN MOD_ASSIGN PLUS_ASSIGN MINUS_ASSIGN LSHIFT_ASSIGN RSHIFT_ASSIGN AND_ASSIGN XOR_ASSIGN OR_ASSIGN 
%token AUTO REGISTER STATIC EXTERN TYPEDEF
%token VOID CHAR SHORT INT LONG FLOAT DOUBLE SIGNED UNSIGNED STRING FUNC
%token CONST VOLATILE
%token STRUCT UNION
%token SWITCH CASE ENUM DEFAULT IF ELSE
%token LBRACKET RBRACKET LBRACKET_SQUARE RBRACKET_SQUARE LPAREN RPAREN COMMA
COLON ELLIPSIS ASTERISK PERIOD
%token WHILE DO FOR GOTO CONTINUE BREAK
%token EXTENDS IMPLEMENTS
%token QUESTION
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%nonassoc DELENIATOR
%start program
%type <Ast.tProgram> program

%%

statement_list:
  /* nothing */ { [] }
  | statement_list statement { $2::$1 } 

statement:
  expr_opt SEMICOLON { Expr $1 }
  | selection_statement { $1 }
  | compound_statement { $1 }
  | iteration_statement { $1 }
  | BREAK SEMICOLON { Break }
  | RETURN expr_opt SEMICOLON { Return $2 }

selection_statement:
    IF LPAREN expr RPAREN statement %prec NOELSE { If($3, $5, EmptyElse) }
  | IF LPAREN expr RPAREN statement ELSE statement  {If($3, $5, $7)}

iteration_statement:
    WHILE LPAREN expr RPAREN statement { While($3, $5) }
  | FOR LPAREN expr_opt SEMICOLON expr_opt SEMICOLON expr_opt RPAREN statement { For($3, $5, $7, $9) }

expr_opt:
  /* Nothing */ {Noexpr}
  | expr          { $1 }

expr:
  assignment_expression { $1 }
  | func_call_expr { $1 }
  | anon_func_def { AnonFuncDef($1) }

assignment_expression:
    IDENTIFIER assignment_operator expr { AsnExpr(Identifier($1), $2, $3) }
  | add_expr { $1 }

postfix_expr:
    primary_expr { $1 }
  | postfix_expr LBRACKET_SQUARE expr RBRACKET_SQUARE { Postfix($1,
  PostEmptyOp,  $3) }
  | postfix_expr PERIOD IDENTIFIER { Postfix($1, PostDeref, Id(Identifier($3))) }
  | postfix_expr PLUS PLUS { Postfix($1, PostPlusPlus, Noexpr) }
  | postfix_expr MINUS MINUS { Postfix($1, PostMinusMinus, Noexpr) }

func_call_expr:
   IDENTIFIER LPAREN expr_list RPAREN  { Call(Id(Identifier($1)), $3) }

expr_list:
  /* Nothing */ { [] }
| expr    { [$1] }
| expr_list COMMA expr { $3 :: $1 }

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

binary_operator:
   AND { And }
 | OR { Or }
 | BITWISE_AND { BitAnd }
 | BITWISE_OR { BitOr }
 | XOR { Xor }
 | NOT { Not }
 | LSHIFT { Lsh }
 | RSHIFT { Rsh }

logical_opeator:
   EQUALS { Eql }
 | NOT_EQUALS { NotEql }
 | LESS_THAN { Less } 
 | LESS_THAN_EQUALS { LessEql }
 | GREATER_THAN { Greater }
 | GREATER_THAN_EQUALS { GreaterEql }

add_expr:
   add_expr PLUS mult_expr { Binop($1, Add, $3) }
 | add_expr MINUS mult_expr { Binop($1, Sub, $3) }
 | mult_expr  { $1 }

mult_expr:
   mult_expr TIMES primary_expr { Binop($1, Mul, $3) }
 | mult_expr DIVIDE primary_expr { Binop($1, Div, $3) }
 | mult_expr MOD primary_expr { Binop($1, Mod, $3) }
 | primary_expr             { $1 }

primary_expr:
 LPAREN expr RPAREN         { $2 }
 | FLOAT_LITERAL            { FloatLiteral($1) }
 | INT_LITERAL              { Literal($1) }
 | STRING_LITERAL           { StringLiteral($1) }
 | IDENTIFIER               { Id(Identifier($1))}
 | BITWISE_AND IDENTIFIER   { Address(Ampersand, Id(Identifier($2))) }

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
 | STRING { String }

storage_class_specifier:
   AUTO   { Auto }  
 | REGISTER { Register }
 | STATIC   { Static }
 | EXTERN  { Extern }
 | TYPEDEF { Typedef }

declaration_specifiers:
   type_ { DeclSpecTypeSpecAny($1) }
 | declaration_specifiers type_ { DeclSpecTypeSpecInitList($2, $1) }
 
type_:
   type_specifier { PrimitiveType($1) }
 | STRUCT STRUCT_IDENTIFIER { CustomType($2) }

init_declarator_list:
   init_declarator { InitDeclList([$1]) }  
 | init_declarator_list COMMA init_declarator { InitDeclList($3::[$1])}

init_declarator:
   declarator  { InitDeclarator($1) }
 | declarator ASSIGN expr { InitDeclaratorAsn($1, Asn, $3) }

pointer:
   TIMES pointer { PtrType(Pointer, $2) }
 | TIMES { Pointer }

declarator:
   direct_declarator { DirectDeclarator($1) }
 | pointer direct_declarator { PointerDirDecl($1, $2) }

direct_declarator:
    IDENTIFIER { Var(Identifier($1)) }


declaration:
   declaration_specifiers init_declarator_list SEMICOLON { Declaration($1, $2)}

declaration_list:
   /* Nothing */ { [] }
   | declaration_list declaration { $2 :: $1 }


struct_declaration:
 STRUCT STRUCT_IDENTIFIER struct_inheritence_opt struct_interface_opt LBRACKET
 declaration_list RBRACKET SEMICOLON { {
         members = (List.rev $6);
         struct_name = $2;
         extends = $3;
         implements = $4;
 } }

struct_inheritence_opt:
  EXTENDS STRUCT_IDENTIFIER { $2 }
  | { "" }

struct_interface_opt:
   | IMPLEMENTS STRUCT_IDENTIFIER { $2 }
   |  { "" }

compound_statement:
     LBRACKET declaration_list statement_list RBRACKET { CompoundStatement((List.rev $2),
     (List.rev $3)) }

func_params:
    declaration_specifiers declarator { FuncParamsDeclared($1, $2) }
  | declaration_specifiers { ParamDeclWithType($1) }
  | anon_func_decl { AnonFuncDecl($1) }


func_params_list:
   /* Nothing */ { [] }
   | func_params { [$1] }
   | func_params_list COMMA func_params { $3 :: $1 }


func_decl:
     declaration_specifiers declarator LPAREN func_params_list RPAREN compound_statement { {
             return_type = $1;
             func_name = $2;
             params = ($4);
             body = $6 }}


anon_func_def:
   FUNC LPAREN RPAREN LPAREN func_params_list RPAREN compound_statement { {
    anon_return_type = PrimitiveType(Void);
    anon_params = ($5);
    anon_body = $7}
   }

 | FUNC LPAREN type_ RPAREN LPAREN func_params_list RPAREN compound_statement { {
    anon_return_type = $3;
    anon_params = ($6);
    anon_body = $8}
 }

anon_func_decl:
   FUNC LPAREN RPAREN LPAREN func_params_list RPAREN IDENTIFIER { {
    anon_decl_return_type = PrimitiveType(Void);
    anon_decl_params = ($5);
    anon_decl_name = Identifier($7);}
  }
 | FUNC LPAREN type_ RPAREN LPAREN func_params_list RPAREN IDENTIFIER { {
    anon_decl_return_type = $3;
    anon_decl_params = ($6);
    anon_decl_name = Identifier($8);}
  }

decls:
   /* Nothing */ { { globals = []; structs = []; functions = [] }}
   | decls func_decl { {functions = $2 :: ($1.functions); globals = $1.globals;
                        structs = $1.structs} }
   | decls declaration { { functions = $1.functions; globals =  ($2 ::
           $1.globals);
   structs = $1.structs }}
   | decls struct_declaration { {functions = $1.functions; globals =
           $1.globals; structs = ($2 :: $1.structs)}}

program:
   decls EOF { $1 }
