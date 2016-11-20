type tOperator = Add | Sub | Mul | Div | Mod | And | Or | BitAnd | BitOr | Xor | Not | Lsh | Rsh

type tAssignmentOperator = Asn | MulAsn | DivAsn | ModAsn | AddAsn | SubAsn |
LshAsn | RshAsn | AndAsn | XorAsn | OrAsn

type tLogicalOperator = Eql | NotEql | Less | LessEql | Greater | GreaterEql

type tPostfixOperator = PostPlusPlus | PostMinusMinus | PostDeref | PostEmptyOp

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
        | String

type tType = 
     PrimitiveType of tTypeSpec
   | CustomType of string
   | CompoundType of tType * tType

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
  | FloatLiteral of float
  | StringLiteral of string
  | Postfix of tExpr * tPostfixOperator * tExpr
  | Call of tIdentifier * tExpr list
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
      | DeclSpecTypeSpecAny of tType
      | DeclSpecTypeSpecInitList of tType * tDeclarationSpecifiers 
      (*| DeclSpecTypeSpecInitList of tTypeSpec * tDeclarationSpecifiers *)

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

type tStruct = {
        members: tDeclaration list;
        struct_name: string;
        extends: string;
        implements: string;
}

type tFuncDecl = {
        return_type: tDeclarationSpecifiers;
        func_name: tDeclarator;
        params: tFuncParam list;
        body: tStatement }

type tProgram = {
        globals: tDeclaration list;
        structs: tStruct list;
        functions: tFuncDecl list;
}

type sSymbol =
    VarSymbol of string * tType 
  | FuncSymbol of string * tType * tType list


