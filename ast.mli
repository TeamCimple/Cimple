type operator = Add | Sub | Mul | Div | Lsh | Rsh | BitAnd | BitXor | BitOr

type assignment_operator = Asn | MulAsn | DivAsn | ModAsn | AddAsn | SubAsn |
LshAsn | RshAsn | AndAsn | XorAsn | OrAsn

type type_qualifier = Const | Volatile

type type_spec = 
        Void 
        | Char 
        | Short 
        | Int 
        | Long 
        | Float 
        | Double 
        | Signed 
        | Unsigned

type identifier = Identifier of string

type pointer = Pointer of type_spec * identifier


type declaration_specifiers = 
        DeclSpecTypeSpec of type_spec
      | DeclSpecTypeSpecInitList of type_spec * declaration_specifiers 

type type_spec_indicator = 
        TypeSpec of type_spec
      | TypeSpecWithDeclSpec of type_spec * declaration_specifiers

type storage_class_spec = Auto | Register | Static | Extern | Typedef

type unary_operator = PlusPlus

type variable =
    SimpleVar of identifier

type declarator = 
    DirectDeclarator of variable

type expr =
  Binop of expr * operator * expr
  | Unop of expr * unary_operator 
  (*| AsnOp of identifier * assignment_operator * expr*)
  | AsnExpr of declarator * assignment_operator * expr 
  | Literal of int
  | Float of float
  | Noexpr



type statement = 
    Block of statement list
  | Expr of expr
  | Return of expr
  | If of expr * statement * statement
  | For of expr * expr * expr * statement
  | While of expr * statement

type init_declarator =
      InitDeclarator of declarator
    | InitDeclList of init_declarator list 
    | InitDeclaratorAsn of declarator * assignment_operator * expr

(*type init_declarator_list = *)
    (*InitDeclSingle of init_declarator list*)
    (*| InitDeclMultiple of init_declarator *)
    (*[>| InitDeclList of init_declarator_list * init_declarator <]*)

type declaration = 
    Declaration of declaration_specifiers
   | DeclarationList of declaration_specifiers * init_declarator 
