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

let string_of_storage_class_spec = function
        Auto -> "auto"
      | Register -> "register"
      | Static -> "static"
      | Extern -> "extern"
      | Typedef -> "typedef"

let string_of_identifier = function
        Identifier(s) -> s

let rec string_of_ptr = function
      PtrType(x, y) -> "Pointer(" ^ string_of_ptr x ^
      string_of_ptr y ^ ")"
      | Pointer -> "Pointer"

let rec string_of_declaration_specifiers = function
        DeclSpecTypeSpec(tspec) -> "DeclSpecTypeSpec(" ^ string_of_type_spec tspec ^ ")"
      | DeclSpecTypeSpecInitList(tspec, idspecs) -> "DeclSpecTypeSpecInitList(" ^ string_of_type_spec tspec ^ ", " ^ string_of_declaration_specifiers idspecs ^ ")" 

let string_of_type_spec_indicator = function
        TypeSpec(tspec) -> "TypeSpec(" ^ string_of_type_spec tspec ^ ")"
      | TypeSpecWithDeclSpec(tspec, declSpec) -> "TypeSpecWithDeclSpec(" ^ string_of_type_spec tspec ^ ", " ^ string_of_declaration_specifiers declSpec ^ ")"     


let string_of_unary_op = function 
        PlusPlus -> "PlusPlus"

let string_of_variable = function
        Var(id) -> "Var(" ^ string_of_identifier id ^ ")"

let string_of_declarator = function
    DirectDeclarator(v) -> string_of_variable v 
   | PointerDirDecl(ptr, decl) -> string_of_ptr ptr ^ "(" ^ string_of_variable
   decl ^ ")"

let rec string_of_expr = function 
   Literal(x) -> "Int(" ^ string_of_int x ^ ")"
  | Float(x) -> "Float(" ^ string_of_float x ^ ")"
  | Id (x) -> "Identifier(" ^ string_of_identifier x ^ ")"
  | Noexpr -> "NOEXPR"
  | AsnExpr(e1, asnOp, e) -> string_of_assignment_op asnOp ^ "(" ^
  string_of_identifier e1 ^  ", " ^ string_of_expr e ^ ")"
  | Binop(e1, op, e2) -> string_of_op op ^ "(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | Unop(e, unOp) -> string_of_unary_op unOp ^ "(" ^ string_of_expr e ^ ")"


let rec string_of_statement = function
   Expr(e) -> "Statement(" ^ string_of_expr e ^ ")"
  | Return(e) -> "RETURN " ^ (string_of_expr e)
  | If(e, s1, s2) -> "IF " ^ (string_of_expr e) ^" " ^ (string_of_statement s1)^ "
  " ^ (string_of_statement s2)
  | For(e1, e2, e3, s) -> "FOR " ^ (string_of_expr e1) ^ " " ^ (string_of_expr
  e2) ^ " " ^ (string_of_expr e3) ^ " " ^ (string_of_statement s)
  | While(e, s) -> "WHILE " ^ (string_of_expr e) ^ " " ^ (string_of_statement s)

let rec string_of_statement_list = function
        [] -> ""
       | h :: t -> string_of_statement h ^ ", " ^ (string_of_statement_list t)

let rec string_of_init_declarator = function
   InitDeclarator(x) -> string_of_declarator x
  | InitDeclList([]) -> ""
  | InitDeclList(h::t) -> let string_of_init_decl_list str
  initdecl =  str ^ (string_of_init_declarator initdecl) in
  string_of_init_declarator h ^ "," ^  (List.fold_left string_of_init_decl_list "" t)


let string_of_declaration = function Declaration(x, y) -> "(" ^ string_of_declaration_specifiers x ^ " " ^
  string_of_init_declarator y ^ ")"
        
let rec string_of_declaration_list = function
   [] -> ""
  | h :: t -> string_of_declaration h ^ ", " ^ (string_of_declaration_list t)

let string_of_compound_statement = function
     (x, y) -> "{\n" ^ "DECL_LIST(" ^ string_of_declaration_list x ^ ")" ^  "\n" ^
     "STMT_LIST(" ^ string_of_statement_list y ^ "STMT_LIST" ^ "\n}"

let string_of_func_param = function
        | FuncParamsDeclared(decl_specs, declarator) ->
                        "PARAM(" ^ string_of_declaration_specifiers
                        decl_specs ^ " " ^
                        string_of_declarator declarator ^ ")"
        | ParamDeclWithType(decl_specs) -> "PARAM(" ^
        string_of_declaration_specifiers decl_specs ^ ")"

let string_of_func fdecl = "FuncDecl(\n" ^ 
      string_of_declaration_specifiers fdecl.return_type ^ "\n" ^
      string_of_declarator fdecl.name ^ "\n" ^ "PARAM_LIST(" ^ String.concat ", " (List.map
      string_of_func_param fdecl.params) ^ ")\n" ^ string_of_compound_statement 
      fdecl.body ^ ")"
        
        
let _ =
       let lexbuf = Lexing.from_channel stdin in
       let func_decl = Parser.func_decl Scanner.token lexbuf in
         Printf.printf "%s\n" (string_of_func func_decl)
