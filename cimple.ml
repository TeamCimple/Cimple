open Ast

let string_of_op = function
        Add -> "PLUS"
      | Sub -> "MINUS"
      | Mul -> "TIMES"
      | Div -> "DIVIDE"
      | Lsh -> "LSHIFT"
      | Rsh -> "RSHIFT"
      | BitAnd -> "BITWISE_AND"
      | BitXor -> "BITWISE_XOR"
      | BitOr -> "BITWISE_OR"

let string_of_assignment_op = function
        Asn -> "EQUALS"
      | MulAsn -> "TIMES_EQUALS"
      | DivAsn -> "DIVIDE_EQUALS"
      | ModAsn -> "MOD_EQUALS"
      | AddAsn -> "ADD_EQUALS"
      | SubAsn -> "MINUS_EQUALS"
      | LshAsn -> "LSHIFT_EQUALS"
      | RshAsn -> "RSHIFT_EQUALS"
      | AndAsn -> "AND_EQUALS"
      | XorAsn -> "XOR_EQUALS"
      | OrAsn -> "OR_EQUALS"

let string_of_type_qualifier = function
        Const -> "const"
      | Volatile -> "volatile"

let string_of_type_spec = function
        Void -> "void"
      | Char -> "char" 
      | Short -> "short"
      | Int -> "int"
      | Long -> "long"
      | Float -> "float"
      | Double -> "double"
      | Signed -> "signed"
      | Unsigned -> "unsigned"

let string_of_identifier = function
        Identifier(s) -> s

let string_of_pointer = function
      Pointer(tspec, id) -> "Pointer(" ^ string_of_type_spec tspec ^ string_of_identifier id ^ ")"     

let rec string_of_declaration_specifiers = function
        DeclSpecTypeSpec(tspec) -> "DeclSpecTypeSpec(" ^ string_of_type_spec tspec ^ ")"
      | DeclSpecTypeSpecInitList(tspec, idspecs) -> "DeclSpecTypeSpecInitList(" ^ string_of_type_spec tspec ^ ", " ^ string_of_declaration_specifiers idspecs ^ ")" 


let rec string_of_expr = function 
   Literal(x) -> string_of_int x
  | Float(x) -> string_of_float x
  | Noexpr -> "NOEXPR"
  (*|  AsnOp(e1, assn_op, e2) -> (match assn_op with*)
                          (*Asn -> e1 ^ "=" ^ (string_of_expr e2)*)
                         (*| MulAsn -> e1 ^ "*=" ^ (string_of_expr e2)*)
                         (*| DivAsn -> e1 ^ "/=" ^ (string_of_expr e2)*)
                         (*| ModAsn -> e1 ^ "%=" ^ (string_of_expr e2)*)
                         (*| AddAsn -> e1 ^ "+=" ^ (string_of_expr e2)*)
                         (*| SubAsn -> e1 ^ "-=" ^ (string_of_expr e2)*)
                         (*| LshAsn -> e1 ^ "<<=" ^ (string_of_expr e2)*)
                         (*| RshAsn -> e1 ^ ">>=" ^ (string_of_expr e2)*)
                         (*| AndAsn -> e1 ^ "&=" ^ (string_of_expr e2)*)
                         (*| XorAsn -> e1 ^ "^=" ^ (string_of_expr e2)*)
                         (*| OrAsn -> e1 ^ "|=" ^ (string_of_expr e2)*)
                         (*)*)
  |  Binop(e1, op, e2) -> match op with 
                           Add -> "Add(" ^ (string_of_expr e1) ^", " ^
                         (string_of_expr e2) ^ ")"
                         | Sub -> "Sub(" ^ (string_of_expr e1) ^", " ^
                         (string_of_expr e2) ^ ")"
                         | Mul-> "Mul(" ^ (string_of_expr e1) ^", " ^
                         (string_of_expr e2) ^ ")"
                         | Div-> "Div(" ^ (string_of_expr e1) ^", " ^
                         (string_of_expr e2) ^ ")"

let rec string_of_statement = function
  | Block([]) -> ""
  |  Block(h::t) -> "list"
  | Expr(e) -> string_of_expr e
  | Return(e) -> "RETURN " ^ (string_of_expr e)
  | If(e, s1, s2) -> "IF " ^ (string_of_expr e) ^" " ^ (string_of_statement s1)^ "
  " ^ (string_of_statement s2)
  | For(e1, e2, e3, s) -> "FOR " ^ (string_of_expr e1) ^ " " ^ (string_of_expr
  e2) ^ " " ^ (string_of_expr e3) ^ " " ^ (string_of_statement s)
  | While(e, s) -> "WHILE " ^ (string_of_expr e) ^ " " ^ (string_of_statement s)

let _ =
       let lexbuf = Lexing.from_channel stdin in
       let statement = Parser.statement Scanner.token lexbuf in
         Printf.printf "%s\n" (string_of_statement statement)       
