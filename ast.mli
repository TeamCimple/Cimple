type operator = Add | Sub | Mul | Div

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
  | Literal of int
  | Float of float
  | Id of string
  | String of string
  | Char of char
  | Noexpr

type statement = 
    Block of statement list
  | Expr of expr
  | Return of expr
  | If of expr * statement * statement
  | For of expr * expr * expr * statement
  | While of expr * statement
