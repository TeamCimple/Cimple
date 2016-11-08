type tOperator = Add | Sub | Mul | Div | Lsh | Rsh | BitAnd | BitXor | BitOr

type tAssignmentOperator = Asn | MulAsn | DivAsn | ModAsn | AddAsn | SubAsn |
LshAsn | RshAsn | AndAsn | XorAsn | OrAsn

type tTypeQualifier = Const | Volatile

type tTypeSpec = 
        Void 
        | Char 
        | Short 
        | Int 
        | Long 
        | Float 
        | Double 
        | Signed 
        | Unsigned

type tIdentifier = Identifier of string

type tPointer = Pointer of tTypeSpec * tIdentifier


type tDeclarationSpecifiers = 
        DeclSpecTypeSpec of tTypeSpec
      | DeclSpecTypeSpecInitList of tTypeSpec * tDeclarationSpecifiers 

type tTypeSpecIndicator = 
        TypeSpec of tTypeSpec
      | TypeSpecWithDeclSpec of tTypeSpec * tDeclarationSpecifiers

type tStorageClassSpec = Auto | Register | Static | Extern | Typedef

type tUnaryOperator = PlusPlus

type tVariable =
    Var of tIdentifier

type tDeclarator = 
    DirectDeclarator of tVariable

type tExpr =
  Binop of tExpr * tOperator * tExpr
  | Unop of tExpr * tUnaryOperator 
  | AsnExpr of tDeclarator * tAssignmentOperator * tExpr 
  | Literal of int
  | Float of float
  | Noexpr

type tStatement = 
    StatementList of tStatement list
  | Expr of tExpr
  | Return of tExpr
  | If of tExpr * tStatement * tStatement
  | For of tExpr * tExpr * tExpr * tStatement
  | While of tExpr * tStatement

 
type tInitDeclarator =
    InitDeclarator of tDeclarator
  | InitDeclList of tInitDeclarator list 
  | InitDeclaratorAsn of tDeclarator * tAssignmentOperator * tExpr

type tDeclaration = 
    Declaration of tDeclarationSpecifiers
   | DeclarationList of tDeclarationSpecifiers * tInitDeclarator 

