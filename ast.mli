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

and tType = 
   PrimitiveType of tTypeSpec
 | CustomType of string
 | AnonFuncType of tType * tType list 

and tIdentifier =
   Identifier of string

and tPointer = 
   PtrType of tPointer * tPointer
 | Pointer

and tStorageClassSpec = Auto | Register | Static | Extern | Typedef

and tUnaryOperator = PlusPlus

and tExpr =
  Binop of tExpr * tOperator * tExpr
  | Unop of tExpr * tUnaryOperator 
  | AsnExpr of tIdentifier * tAssignmentOperator * tExpr 
  | Literal of int
  | FloatLiteral of float
  | StringLiteral of string
  | Postfix of tExpr * tPostfixOperator * tExpr
  | Call of tExpr * tExpr list
  | Id of tIdentifier
  | AnonFuncDef of tAnonFuncDef
  | DeclExpr of tDeclaration
  | Noexpr

 and tDirectDeclarator = 
  Var of tIdentifier
  | ArrDirDecl of tDirectDeclarator * tExpr

 and tDeclarator = 
  PointerDirDecl of tPointer * tDirectDeclarator
  | DirectDeclarator of tDirectDeclarator
  (*| NullDeclarator*)

 and tInitDeclarator =
    InitDeclarator of tDeclarator
  | InitDeclList of tInitDeclarator list 
  | InitDeclaratorAsn of tDeclarator * tAssignmentOperator * tExpr

 and tDeclarationSpecifiers = 
    DeclSpecTypeSpec of tTypeSpec
  | DeclSpecTypeSpecAny of tType
  | DeclSpecTypeSpecInitList of tType * tDeclarationSpecifiers 

 and tTypeSpecIndicator = 
    TypeSpec of tTypeSpec
  | TypeSpecWithDeclSpec of tTypeSpec * tDeclarationSpecifiers

 and tFuncParam = 
   FuncParamsDeclared of tDeclarationSpecifiers * tDeclarator
  |ParamDeclWithType of tDeclarationSpecifiers
  |AnonFuncDecl of tAnonFuncDecl

 and  tFuncParamList = tFuncParam list

 and tDeclaration = 
   Declaration of tDeclarationSpecifiers * tInitDeclarator

 and tStatement = 
   Expr of tExpr
 | EmptyElse
 | Return of tExpr
 | CompoundStatement of tDeclaration list * tStatement list
 | If of tExpr * tStatement * tStatement
 | For of tExpr * tExpr * tExpr * tStatement
 | While of tExpr * tStatement
 | Break

 and tAnonFuncDef = {
        anon_return_type: tType;
        anon_params: tFuncParam list;
        anon_body: tStatement 
 }

and tAnonFuncDecl = {
        anon_decl_return_type: tType;
        anon_decl_params: tFuncParam list;
        anon_decl_name: tIdentifier;
}

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
  | AnonFuncSymbol of string * tType 
