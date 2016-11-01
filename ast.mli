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

type storage_class_spec = Auto | Register | Static | Extern | Typedef

type expr =
  Binop of expr * operator * expr
  | AsnOp of string * assignment_operator * expr
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

