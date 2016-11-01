open Ast

let rec prepend_tabs n str = match n with
   0 -> str 
 | 1 -> "----" ^ str 
 | _ -> prepend_tabs (n - 1) (("----" ^ str ))

let string_of_int_n = function
    (n, i) -> prepend_tabs (n) (string_of_int i)

let string_of_float_n = function
    (n, f) -> prepend_tabs (n) (string_of_float f) 


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

let string_of_op_n = function
   (n, op) -> "\n" ^ (prepend_tabs n (string_of_op op))

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

let string_of_assignment_op_n = function
  | (n, op) -> "\n" ^ (prepend_tabs n (string_of_assignment_op op))

let rec string_of_expr = function 
    Binop(e1, op, e2) -> (string_of_expr e1) ^ " " ^(string_of_op op) ^ " " ^
    (string_of_expr e2)
  | AsnOp(s, asop, e) -> s ^ " " ^ (string_of_assignment_op asop) ^ " " ^
  (string_of_expr e)
  | Literal(x) -> string_of_int x
  | Float(x) -> string_of_float x
  | Noexpr -> "NOEXPR"

let rec string_of_expr_n = function
    (n, Binop(e1, op, e2)) -> let str1 = (string_of_expr_n ((n+1), e1))
                            and str2 = (string_of_op_n ((n+1), op))
                            and str3 = (string_of_expr_n ((n+1), e2)) in
                                "\n" ^ (prepend_tabs n (str1 ^ str2 ^ str3))
  | (n, AsnOp(s, asop, e)) -> let str1 = (string_of_assignment_op_n ((n), asop))
                              and str2 = (string_of_expr_n ((n), e)) in
                                "\n" ^ (prepend_tabs n (s ^ str1 ^ str2))
  | (n, Literal(x)) -> let str1 = (string_of_int_n ((n+1), x)) in
                        "\n" ^ (prepend_tabs n str1)
  | (n, Float(x)) -> let str1 = (string_of_float_n ((n+1), x)) in
                        "\n" ^ (prepend_tabs n str1)
  | (n, Noexpr) -> prepend_tabs n "NOEXPR" 

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

let rec string_of_statement_n = function
    (n, Block([])) -> ""
  | (n, Block(h::t)) -> ""
  | (n, Expr(e)) -> let str = (string_of_expr_n ((n+1), e)) in
                        "\n" ^ (prepend_tabs n str)
  | (n, Return(e)) -> let str = (string_of_expr_n ((n+1), e)) in
                        "\n" ^ (prepend_tabs n ("RETURN " ^ str))
  | (n, If(e, s1, s2)) -> let str1 = (string_of_expr_n ((n+1), e))
                          and str2 =(string_of_statement_n ((n+1), s1))
                          and str3 = (string_of_statement_n ((n+1), s2)) in
                         "\n" ^ (prepend_tabs n ("IF " ^ str1 ^ str2 ^ str3))
  | (n, For(e1, e2, e3, s)) -> let str1 = (string_of_expr_n ((n+1), e1))
                              and str2 = (string_of_expr_n ((n+1), e2))
                              and str3 = (string_of_expr_n ((n+1), e3))
                              and str4 = (string_of_statement_n ((n+1), s)) in
                                "\n" ^ (prepend_tabs n ("FOR " ^ str1 ^ str2 ^
                                str3 ^ str4))
  | (n, While(e, s)) -> let str1 = (string_of_expr_n ((n+1), e))
                        and str2 = (string_of_statement_n ((n+1), s)) in
                        "\n" ^ (prepend_tabs n ("WHILE " ^ str1 ^ str2))

let _ =
       let lexbuf = Lexing.from_channel stdin in
       let statement = Parser.statement Scanner.token lexbuf in
         Printf.printf "%s\n" (string_of_statement_n (0, statement))       
