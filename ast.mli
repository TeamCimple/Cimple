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

type tPointer = 
        PtrType of tPointer * tPointer
        | Pointer

type tStorageClassSpec = Auto | Register | Static | Extern | Typedef

type tUnaryOperator = PlusPlus

type tExpr =
  Binop of tExpr * tOperator * tExpr
  | Unop of tExpr * tUnaryOperator 
  | AsnExpr of tIdentifier * tAssignmentOperator * tExpr 
  | Literal of int
  | Float of float
  | Id of tIdentifier
  | Noexpr

type tDirectDeclarator = 
        Var of tIdentifier
        | ArrDirDecl of tDirectDeclarator * tExpr

type tDeclarator = 
        PointerDirDecl of tPointer * tDirectDeclarator
        | DirectDeclarator of tDirectDeclarator

type tInitDeclarator =
    InitDeclarator of tDeclarator
  | InitDeclList of tInitDeclarator list 
  | InitDeclaratorAsn of tDeclarator * tAssignmentOperator * tExpr

type tDeclarationSpecifiers = 
        DeclSpecTypeSpec of tTypeSpec
      | DeclSpecTypeSpecInitList of tTypeSpec * tDeclarationSpecifiers 

type tDeclaration = 
   Declaration of tDeclarationSpecifiers * tInitDeclarator

type tDeclarationList = tDeclaration list

type tTypeSpecIndicator = 
        TypeSpec of tTypeSpec
      | TypeSpecWithDeclSpec of tTypeSpec * tDeclarationSpecifiers

type tFuncParam = 
      FuncParamsDeclared of tDeclarationSpecifiers * tDeclarator
      |ParamDeclWithType of tDeclarationSpecifiers

type tFuncParamList = tFuncParam list

type tStatement = 
    Expr of tExpr
  | EmptyElse
  | Return of tExpr
  | CompoundStatement of tDeclarationList * tStatement list
  | If of tExpr * tStatement * tStatement
  | For of tExpr * tExpr * tExpr * tStatement
  | While of tExpr * tStatement

type tFuncDecl = {
        return_type: tDeclarationSpecifiers;
        name: tDeclarator;
        params: tFuncParamList;
        body: tStatement }
