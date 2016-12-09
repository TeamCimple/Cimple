open Ast

let string_of_op = function
        Add -> "PLUS"
      | Sub -> "MINUS"
      | Mul -> "TIMES"
      | Div -> "DIVIDE"
      | Mod -> "MOD"
      | And -> "AND"
      | Or -> "OR"
      | BitAnd -> "BITWISE_AND"
      | BitOr -> "BITWISE_OR"
      | Xor -> "XOR"
      | Not -> "NOT"
      | Lsh -> "LSHIFT"
      | Rsh -> "RSHIFT"
     
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

let string_of_logical_op = function
        Eql -> "EQUALS"
      | NotEql -> "NOT_EQUALS"
      | Less -> "LESS_THAN"
      | LessEql -> "LESS_THAN_EQUALS"
      | Greater -> "GREATER_THAN"
      | GreaterEql -> "GREATER_THAN_EQUALS"
 
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
      | String -> "string"

let rec string_of_type = function
      PrimitiveType(t) -> string_of_type_spec t
    | PointerType(t, d) -> string_of_type t ^ " depth: " ^ string_of_int d
    | CustomType(t) -> t

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
      | DeclSpecTypeSpecInitList(t, idspecs) -> "DeclSpecTypeSpecInitList(" ^ string_of_type t ^ ", " ^ string_of_declaration_specifiers idspecs ^ ")" 
      | DeclSpecTypeSpecAny(t) -> string_of_type t

let string_of_type_spec_indicator = function
        TypeSpec(tspec) -> "TypeSpec(" ^ string_of_type_spec tspec ^ ")"
      | TypeSpecWithDeclSpec(tspec, declSpec) -> "TypeSpecWithDeclSpec(" ^ string_of_type_spec tspec ^ ", " ^ string_of_declaration_specifiers declSpec ^ ")"     


let string_of_unary_op = function 
        PlusPlus -> "PlusPlus"

let string_of_variable = function
        Var(id) -> "Var(" ^ string_of_identifier id ^ ")"

let string_of_declarator = function
    DirectDeclarator(v) -> string_of_variable v 
   | PointerDirDecl(ptr, decl) -> string_of_ptr ptr ^ "("
   ^ string_of_variable decl ^ ")"

let string_of_receiver receiver = 
        match (receiver) with 
        ("", "") -> ""
        | (d, u) -> "RECEIVER("^d^","^u^") "

let rec string_of_expr = function 
   Literal(x) -> "Int(" ^ string_of_int x ^ ")"
  | FloatLiteral(x) -> "Float(" ^ string_of_float x ^ ")"
  | StringLiteral(s) -> "String(" ^ s ^ ")" 
  | Id (x) -> "Identifier(" ^ string_of_identifier x ^ ")"
  | Pointify(e) -> "Pointify(" ^ string_of_expr e ^ ")"
  | Noexpr -> ""
  | AsnExpr(e1, asnOp, e) -> string_of_assignment_op asnOp ^ "(" ^
  string_of_identifier e1 ^  ", " ^ string_of_expr e ^ ")"
  | Binop(e1, op, e2) -> string_of_op op ^ "(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | Unop(e, unOp) -> string_of_unary_op unOp ^ "(" ^ string_of_expr e ^ ")"
  | Call(obj, Id(id), exprList) -> "Call(" ^ "Receiver(" ^ obj ^")"  ^
  "FunctionName: " ^ (string_of_identifier id) ^ " Params: " ^ (string_of_expr_list  exprList) ^ ")"
  | Make(typ_, exprList) -> "Make(" ^ string_of_type typ_ ^ string_of_expr_list
  exprList ^ ")" 
  | MemAccess(Identifier(s), Identifier(t)) -> "Deref(" ^ "Var(" ^ s ^ ")" ^ ","
  ^ t ^")"
  | AnonFuncDef(anonDef) -> "AnonFuncDef(ReturnType: " ^ (string_of_type anonDef.anon_return_type) ^ ", Params: " ^ (string_of_func_param_list anonDef.anon_params) ^ ", Body: " ^ (string_of_statement anonDef.anon_body) ^ ")" 

and string_of_func_param_list = function
    [] -> ""
  | [x] -> string_of_func_param x
  | h::t -> (string_of_func_param h) ^ ", " ^ (string_of_func_param_list t)

and string_of_expr_list = function
    [] -> ""
  | [e] -> string_of_expr e
  | h::t -> string_of_expr h ^ string_of_expr_list t

  and  string_of_init_declarator = function
   InitDeclarator(x) -> string_of_declarator x
  | InitDeclList([]) -> ""
  | InitDeclList(h::t) -> let string_of_init_decl_list str
  initdecl =  str ^ (string_of_init_declarator initdecl) in
  string_of_init_declarator h ^ "," ^  (List.fold_left string_of_init_decl_list
  "" t)
  | InitDeclaratorAsn(decl, asnop, expr) -> string_of_assignment_op asnop ^ "("
  ^ string_of_declarator decl ^ " " ^ string_of_expr expr

and string_of_anon_func_decl d = "AnonFuncDecl(Name: " ^ (string_of_identifier d.anon_decl_name) ^ ", ReturnType: " ^ (string_of_type d.anon_decl_return_type) ^ ", Params: " ^ (string_of_func_param_list d.anon_decl_params) ^ ")"

and string_of_declaration = function Declaration(x, y) -> "(" ^ string_of_declaration_specifiers x ^ " " ^
  string_of_init_declarator y ^ ")"
        
and string_of_declaration_list = function
   [] -> ""
  | h :: t -> string_of_declaration h ^ ", " ^ (string_of_declaration_list t)

and string_of_statement = function
 Expr(e) -> "Statement(" ^ string_of_expr e ^ ")"
 | Return(e) -> "RETURN(" ^ (string_of_expr e) ^")"
 | If(e, s1, s2) -> "IF " ^ (string_of_expr e) ^" " ^ (string_of_statement s1)^ "
 " ^ (string_of_statement s2)
 | EmptyElse -> ""
 | For(e1, e2, e3, s) -> "FOR " ^ (string_of_expr e1) ^ " " ^ (string_of_expr
 e2) ^ " " ^ (string_of_expr e3) ^ " " ^ (string_of_statement s)
 | While(e, s) -> "WHILE " ^ (string_of_expr e) ^ " " ^ (string_of_statement s)
 | CompoundStatement(dl, sl) -> "CompoundStatement(Declarations: "  ^ string_of_declaration_list dl ^ " " ^"StatementList: " ^ String.concat ", " (List.map string_of_statement sl) ^ ")"


and string_of_statement_list = function
        [] -> ""
       | h :: t -> string_of_statement h ^ ", " ^ (string_of_statement_list t)

and string_of_func_param = function
        FuncParamsDeclared(decl_specs, declarator) ->
                        "PARAM(" ^ string_of_declaration_specifiers
                        decl_specs ^ " " ^
                        string_of_declarator declarator ^ ") "
        | ParamDeclWithType(decl_specs) -> "PARAM(" ^
        string_of_declaration_specifiers decl_specs ^ ") "
        | AnonFuncDecl(afd) -> string_of_anon_func_decl afd 


let string_of_constructor constructor = "Constructor(" ^ constructor.constructor_name ^ "Body:
        " ^ string_of_statement (constructor.constructor_body) ^ ")"

let string_of_func fdecl = "FuncDecl(Name: " ^ 
      string_of_declarator fdecl.func_name ^ " ReturnType: " ^
      string_of_receiver fdecl.receiver ^ string_of_declaration_specifiers fdecl.return_type ^ " Parameters: " ^
      String.concat ", " (List.map
      string_of_func_param fdecl.params) ^ " Body: " ^ string_of_statement 
      fdecl.body ^ ") "

let string_of_struct struct_decl = "Struct(" ^
        string_of_declaration_list struct_decl.members ^ ", " ^
        struct_decl.struct_name ^ ", " ^ (string_of_constructor
        struct_decl.constructor) ^ "," ^  struct_decl.extends ^ ", " ^
        struct_decl.implements ^ ")"

let string_of_list_objs f list_objs = String.concat ", " (List.map f list_objs) 

let string_of_interface interface = "INTERFACE(" ^ interface.name ^
(string_of_list_objs string_of_func interface.funcs) ^ ")"

let string_of_program program =  
        string_of_declaration_list program.globals  ^ (string_of_list_objs
        string_of_interface program.interfaces) ^ (string_of_list_objs
        string_of_struct program.structs) ^ (string_of_list_objs string_of_func
        program.functions)

let string_of_symbol = function 
   VarSymbol(s, t) -> "Variable_Symbol(Name: " ^ s ^ ", Type: " ^ string_of_type t ^ ")"
 | FuncSymbol(s, fdecl) -> "Function_Symbol(Name: " ^ s ^ ")" (* Finish me! *)
 | StructSymbol(s, strct) -> "Struct_Symbol" (* Finish me! *)
 | InterfaceSymbol(s, ti) -> "Interface_Symbol" (* Finish me! *)
 | AnonFuncSymbol(s, t) -> "AnonymousFunction_Symbol(Name: " ^ s ^ ", Type: " ^ string_of_type t ^ ")"

let rec string_of_symbol_list l = match l with
    [] -> ""
  | [x] -> string_of_symbol x
  | h::t -> string_of_symbol h ^ string_of_symbol_list t
 


