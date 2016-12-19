%{ open Ast %}

%token SEMICOLON
%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <string> STRING_LITERAL
%token <string> IDENTIFIER
%token <string> STRUCT_IDENTIFIER
%token ASSIGN
%token RETURN
%token NIL
%token PLUS MINUS TIMES DIVIDE MOD PLUSPLUS MINUSMINUS
%token AND OR BITWISE_AND BITWISE_OR XOR NOT LSHIFT RSHIFT
%token EQUALS NOT_EQUALS LESS_THAN LESS_THAN_EQUALS GREATER_THAN GREATER_THAN_EQUALS
%token TIMES_ASSIGN DIVIDE_ASSIGN MOD_ASSIGN PLUS_ASSIGN MINUS_ASSIGN LSHIFT_ASSIGN RSHIFT_ASSIGN AND_ASSIGN XOR_ASSIGN OR_ASSIGN 
%token AUTO REGISTER STATIC EXTERN TYPEDEF
%token VOID CHAR SHORT INT LONG FLOAT DOUBLE SIGNED UNSIGNED STRING FUNC
%token CONST VOLATILE
%token STRUCT UNION INTERFACE MAKE SUPER CLEAN
%token SWITCH CASE ENUM DEFAULT IF ELSE
%token LBRACKET RBRACKET LBRACKET_SQUARE RBRACKET_SQUARE LPAREN RPAREN COMMA
COLON ELLIPSIS ASTERISK PERIOD
%token WHILE DO FOR GOTO CONTINUE BREAK
%token EXTENDS IMPLEMENTS
%token QUESTION
%token EOF

%nonassoc NOELSE
%nonassoc NOCALL
%nonassoc ELSE
%nonassoc DELENIATOR
%nonassoc LPAREN
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
    IF LPAREN expr RPAREN statement %prec NOELSE{ If($3, $5, EmptyElse)}
  | IF LPAREN expr RPAREN statement ELSE statement  {If($3, $5, $7)}

iteration_statement:
    WHILE LPAREN expr RPAREN statement { While($3, $5) }
  | FOR LPAREN expr_opt SEMICOLON expr_opt SEMICOLON expr_opt RPAREN statement { For($3, $5, $7, $9) }

expr_opt:
  /* Nothing */ {Noexpr}
  | expr          { $1 }

expr:
  assignment_expression { $1 }
  | make_expr { $1 }
  | anon_func_def { AnonFuncDef($1) }

assignment_expression:
    postfix_expr assignment_operator expr { AsnExpr($1, $2, $3) }
  | logical_or_expression { $1 }


logical_or_expression:
  | logical_and_expression { $1 }
  | logical_or_expression OR logical_and_expression { CompareExpr($1, LogicalOr,
  $3) }

logical_and_expression:
  | equality_expression { $1 }
  | logical_and_expression AND equality_expression { CompareExpr($1, LogicalAnd,
  $3) }

equality_expression:
  | relational_expression { $1 }
  | equality_expression EQUALS relational_expression { CompareExpr($1, Eql,
  $3) }
  | equality_expression NOT_EQUALS relational_expression { CompareExpr($1,
  NotEql, $3) }

relational_expression:
  | add_expr { $1 }
  | relational_expression LESS_THAN add_expr { CompareExpr($1, Less, $3) }
  | relational_expression GREATER_THAN add_expr { CompareExpr($1, Greater, $3) }
  | relational_expression LESS_THAN_EQUALS add_expr { CompareExpr($1, LessEql,
  $3) }
  | relational_expression GREATER_THAN_EQUALS add_expr { CompareExpr($1,
  GreaterEql, $3) } 

unary_expr:
  | postfix_expr { $1 }
  | MINUS postfix_expr { Neg($2) }

postfix_expr:
    primary_expr { $1 }
  | postfix_expr PLUSPLUS { Postfix($1, PostPlusPlus) }
  | postfix_expr MINUSMINUS { Postfix($1, PostMinusMinus) }
  | postfix_expr LPAREN expr_list RPAREN { Call(Noexpr, $1, $3) }
  | postfix_expr LBRACKET_SQUARE postfix_expr RBRACKET_SQUARE { ArrayAccess($1, $3) } 
  | SUPER LPAREN expr_list RPAREN { Super($3) }
  | CLEAN primary_expr { Clean($2) }
  | postfix_expr PERIOD  IDENTIFIER LPAREN expr_list RPAREN  { Call($1,
  Id(Identifier($3)), $5) }
  | postfix_expr PERIOD IDENTIFIER %prec NOCALL{ MemAccess($1, Identifier($3)) }

make_expr:
        MAKE STRUCT_IDENTIFIER LPAREN expr_list RPAREN { Make(CustomType($2),
        $4)}
   | MAKE type_ LBRACKET_SQUARE primary_expr RBRACKET_SQUARE  {
           Make(ArrayType($2, NoPointer, $4), []) }
   | MAKE type_ pointer LBRACKET_SQUARE primary_expr RBRACKET_SQUARE {
           Make(ArrayType($2, $3, $5), [])}

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
   mult_expr TIMES unary_expr { Binop($1, Mul, $3) }
 | mult_expr DIVIDE unary_expr { Binop($1, Div, $3) }
 | mult_expr MOD unary_expr { Binop($1, Mod, $3) }
 | unary_expr             { $1 }

primary_expr:
 LPAREN expr RPAREN         { $2 }
 | FLOAT_LITERAL            { FloatLiteral($1) }
 | INT_LITERAL              { Literal($1) }
 | STRING_LITERAL           { StringLiteral($1) }
 | IDENTIFIER               { Id(Identifier($1))}
 | NIL                      { Nil } 
 | BITWISE_AND primary_expr { Pointify($2) }
 | TIMES primary_expr       { Deref($2) }

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
 | INTERFACE STRUCT_IDENTIFIER { CustomType($2) }

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

record_initializer_list:
   | record_initializer { Initializer($1) } 
   | record_initializer_list COMMA record_initializer {InitializerList($1 ::
           [$3])}

record_initializer:
   assignment_expression { InitializerExpr($1) }
   | LBRACKET record_initializer_list RBRACKET { $2 }
   | LBRACKET record_initializer_list COMMA RBRACKET { $2 }

direct_declarator:
    IDENTIFIER { Var(Identifier($1)) }
    | direct_declarator LBRACKET_SQUARE INT_LITERAL RBRACKET_SQUARE
    { ArrDirDecl($1, $3) }
    | direct_declarator LBRACKET_SQUARE RBRACKET { ArrDirDecl($1, -1) }

declaration:
   declaration_specifiers init_declarator_list SEMICOLON { Declaration($1, $2)}

declaration_list:
   /* Nothing */ { [] }
   | declaration_list declaration { $2 :: $1 }


struct_declaration:
 STRUCT STRUCT_IDENTIFIER struct_inheritence_opt struct_interface_opt LBRACKET
 declaration_list constructor_destructor_opt RBRACKET SEMICOLON { {
         members = (List.rev $6);
         struct_name = $2;
         extends = $3;
         children = [""];
         methods = []; 
         implements = $4;
         constructor = (fst $7);
         destructor = (snd $7);
 }}

struct_inheritence_opt:
  EXTENDS STRUCT_IDENTIFIER { $2 }
  | { "" }

struct_interface_opt:
   | IMPLEMENTS STRUCT_IDENTIFIER { $2 }
   |  { "" }

interface:
   INTERFACE STRUCT_IDENTIFIER LBRACKET func_decl_list RBRACKET SEMICOLON{{
                             name = $2;
                             funcs = $4;
                        }} 

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


receiver:
        STRUCT_IDENTIFIER IDENTIFIER {($1, $2)}
        | STRUCT_IDENTIFIER TIMES IDENTIFIER {($1, $3)}
        | {("", "")}

constructor_destructor_opt: 
     STRUCT_IDENTIFIER LPAREN func_params_list RPAREN compound_statement NOT
     STRUCT_IDENTIFIER LPAREN RPAREN compound_statement{({
             constructor_name = $1;
             constructor_params = $3;
             constructor_body = $5; 
     }, {destructor_name = $1; destructor_body = $10})}
     | /* Nothing */ {({constructor_name = ""; constructor_params = [];
     constructor_body = CompoundStatement([],
     [])}, {destructor_name = ""; destructor_body = CompoundStatement([], [])})}

func_decl:
     declaration_specifiers declarator LPAREN func_params_list RPAREN compound_statement { {
             return_type = $1;
             func_name = $2;
             receiver = ("", "");
             params = ($4);
             body = $6 }}
     | declaration_specifiers LPAREN receiver RPAREN declarator LPAREN
     func_params_list RPAREN compound_statement {{
             return_type = $1;
             func_name = $5;
             receiver = $3;
             params = $7;
             body = $9
     }}
     | declaration_specifiers LPAREN receiver RPAREN declarator
     LPAREN func_params_list RPAREN SEMICOLON {{
            return_type = $1;
            func_name = $5;
            receiver = $3;
            params = $7;
            body = CompoundStatement([], [])
     }}
     | declaration_specifiers declarator LPAREN func_params_list RPAREN
     SEMICOLON {{
            return_type = $1;
            func_name = $2;
            receiver = ("", "");
            params = $4;
            body = CompoundStatement([], [])
     }}

func_decl_list:
        /*Nothing */ { [] }
        | func_decl_list  func_decl { $2::$1 }

anon_func_def:
   FUNC LPAREN RPAREN LPAREN func_params_list RPAREN compound_statement { {
    anon_name = "";
    anon_return_type = PrimitiveType(Void);
    anon_params = ($5);
    anon_body = $7}
   }

 | FUNC LPAREN type_ RPAREN LPAREN func_params_list RPAREN compound_statement { {
    anon_name = "";
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
        /* Nothing */ { { globals = []; structs = []; functions = []; interfaces
        = [] }}
   | decls func_decl { {functions = $2 :: ($1.functions); globals = $1.globals;
   structs = $1.structs; interfaces = $1.interfaces} }
   | decls declaration { { functions = $1.functions; globals =  ($2 ::
           $1.globals); interfaces = $1.interfaces;
   structs = $1.structs }}
   | decls struct_declaration { {functions = $1.functions; globals =
           $1.globals; interfaces = $1.interfaces; structs = ($2 :: $1.structs)}}
   | decls interface { {functions = $1.functions; globals = $1.globals; structs
   = $1.structs; interfaces = $2 :: ($1.interfaces) } }

program:
   decls EOF { $1 }
