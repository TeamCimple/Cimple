open Ast

module StringMap = Map.Make(String)

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

let string_of_postfix_op = function 
      | PostPlusPlus -> "++"
      | PostMinusMinus -> "--"
      | PostEmptyOp -> ""

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
  | Deref(e) -> "Deref(" ^ string_of_expr e ^ ")"
  | Pointify(e) -> "Pointify(" ^ string_of_expr e ^ ")"
  | Postfix(e1, op) -> "Postfix(" ^ string_of_expr e1 ^ "," ^
  (string_of_postfix_op op) ^ ")" 
  | CompareExpr(e1, op, e2) -> "Compare(" ^ string_of_expr e1 ^ "," ^
  string_of_logical_op op ^ "," ^ string_of_expr e2 ^ ")"
  | Noexpr -> ""
  | AsnExpr(e1, asnOp, e) -> string_of_assignment_op asnOp ^ "(" ^
  string_of_expr e1 ^  ", " ^ string_of_expr e ^ ")"
  | Binop(e1, op, e2) -> string_of_op op ^ "(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | Unop(e, unOp) -> string_of_unary_op unOp ^ "(" ^ string_of_expr e ^ ")"
  | Call(e, Id(id), exprList) -> "Call(" ^ "Receiver(" ^ string_of_expr e ^")"  ^
  "FunctionName: " ^ (string_of_identifier id) ^ " Params: " ^ (string_of_expr_list  exprList) ^ ")"
  | Make(typ_, exprList) -> "Make(" ^ string_of_type typ_ ^ string_of_expr_list
  exprList ^ ")" 
  | MemAccess(e, Identifier(t)) -> "Access(" ^ "Var(" ^ string_of_expr e ^ ")" ^ ","
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

and string_of_anon_def d = "AnonDef(AnonName: " ^ d.anon_name ^ ", AnonReturnType: " ^ string_of_type d.anon_return_type ^ ", AnonParams: " ^ string_of_func_param_list d.anon_params ^ ", AnonBody: " ^ string_of_statement d.anon_body ^ ")"

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

let string_of_symbol_simple = function
   VarSymbol(s, t) -> s
 | FuncSymbol(s, fdecl) -> s
 | StructSymbol(s, strct) -> s
 | InterfaceSymbol(s, ti) -> s
 | AnonFuncSymbol(s, t) -> s

let rec string_of_symbol_list l = match l with
    [] -> ""
  | [x] -> string_of_symbol x
  | h::t -> string_of_symbol h ^ string_of_symbol_list t
 

let rec string_of_func_decl_list dlist = match dlist with
    [] -> "\n"
  | [x] -> (string_of_func x) ^ "\n\n"
  | h::t -> (string_of_func h) ^ "\n\n" ^ (string_of_func_decl_list t) ^ "\n\n"



let apply_name_to_anon_def (prefix, count) adef = {
    anon_name =  prefix ^ "_" ^ (string_of_int count);
    anon_return_type = adef.anon_return_type;
    anon_params = adef.anon_params;
    anon_body = adef.anon_body;
}

let rec anon_defs_from_expr (prefix, count) expr = match expr with
     AnonFuncDef(anonDef) ->([(apply_name_to_anon_def (prefix, count) anonDef)], (count + 1))
   | Binop(e1, op, e2) -> 
           let (defs1, count1) = (anon_defs_from_expr (prefix, count) e1) in
           let (defs2, count2) = (anon_defs_from_expr (prefix, count1) e2) in
           (defs1@defs2, count2)
   | AsnExpr(_, _, e) -> anon_defs_from_expr (prefix, count) e
   | Postfix(e1, _) -> (anon_defs_from_expr (prefix, count) e1)
   | Call(_, e, elist) -> 
           let (defs1, count1) = (anon_defs_from_expr (prefix, count) e) in
           let (defs2, count2) = (anon_defs_from_expr_list (prefix, count1) elist) in
           (defs1@defs2, count2);
   | _ -> ([], count) (* Other expression types cannot possibly contain anonymous function definitions *) 

and anon_defs_from_expr_list (prefix, count) elist = match elist with  
     [] -> ([], count)
   | [e] -> anon_defs_from_expr (prefix, count) e
   | h::t ->
           let (defs1, count1) = (anon_defs_from_expr (prefix, count) h) in
           let (defs2, count2) = (anon_defs_from_expr_list (prefix, count1) t) in
           (defs1@defs2, (count2))


let rec anon_defs_from_declaration (prefix, count) decl = match decl with
     Declaration(declSpecs, initDecl) -> anon_defs_from_init_declarator (prefix, count) initDecl

and anon_defs_from_declaration_list (prefix, count) declList = match declList with
     [] -> ([], count)
   | [d] -> anon_defs_from_declaration (prefix, count) d
   | h::t ->
           let (defs1, count1) = (anon_defs_from_declaration (prefix, count) h) in
           let (defs2, count2) = (anon_defs_from_declaration_list (prefix, count1) t) in
           (defs1@defs2, count2)

and anon_defs_from_init_declarator (prefix, count) idecl = match idecl with
           InitDeclaratorAsn(_, _, e) -> anon_defs_from_expr (prefix, count) e
         | InitDeclList(initDeclList) -> anon_defs_from_init_declarator_list (prefix, count) initDeclList
         | _ -> ([], count)

and anon_defs_from_init_declarator_list (prefix, count) ideclList = match ideclList with
     [] -> ([], count)
   | [decl] -> anon_defs_from_init_declarator (prefix, count) decl
   | h::t -> 
           let (defs1, count1) = (anon_defs_from_init_declarator (prefix, count) h) in
           let (defs2, count2) = (anon_defs_from_init_declarator_list (prefix, count1) t) in
           (defs1@defs2, (count2))

let rec anon_defs_from_statement (prefix, count) stmt = match stmt with
     Expr(e) -> anon_defs_from_expr (prefix, count) e
   | Return(e) -> anon_defs_from_expr (prefix, count) e
   | If(e, s1, s2) -> 
           let (defs1, count1) = (anon_defs_from_expr (prefix, count) e) in
           let (defs2, count2) = (anon_defs_from_statement (prefix, count1) s1) in
           let (defs3, count3) = (anon_defs_from_statement (prefix, count2) s2) in
           (defs1@defs2@defs3, count3)
   | For(e1, e2, e3, s) -> 
           let (defs1, count1) = (anon_defs_from_expr (prefix, count) e1) in
           let (defs2, count2) = (anon_defs_from_expr (prefix, count1) e2) in
           let (defs3, count3) = (anon_defs_from_expr (prefix, count2) e3) in
           let (defs4, count4) = (anon_defs_from_statement (prefix, count3) s) in
           (defs1@defs2@defs3@defs4, count4)        
   | While(e, s) -> 
           let (defs1, count1) = (anon_defs_from_expr (prefix, count) e) in
           let (defs2, count2) = (anon_defs_from_statement (prefix, count1) s) in
           (defs1@defs2, count2)
   | CompoundStatement(declList, stmtList) -> 
           let (defs1, count1) = (anon_defs_from_declaration_list (prefix, count) declList) in
           let (defs2, count2) = (anon_defs_from_statement_list (prefix, count1) stmtList) in
           (defs1@defs2, count2) 

and anon_defs_from_statement_list (prefix, count) stmtList = match stmtList with
     [] -> ([], count)
   | [s] -> anon_defs_from_statement (prefix, count) s
   | h::t ->
           let (defs1, count1) = (anon_defs_from_statement (prefix, count) h) in
           let (defs2, count2) = (anon_defs_from_statement_list (prefix, count1) t) in
           (defs1@defs2, count2) 


let rec anon_defs_from_func_decl (prefix, count) fdecl = 
    let newPrefix = 
        (match fdecl.func_name with
              DirectDeclarator(Var(Identifier(s))) -> "a_" ^ s
            | PointerDirDecl(_, Var(Identifier(s))) -> "a_" ^ s)
    in
    anon_defs_from_statement (newPrefix, 0) fdecl.body

and anon_defs_from_func_decl_list (prefix, count) fdlist = match fdlist with
      [] -> ([], count)
    | [x] -> anon_defs_from_func_decl (prefix, count) x
    | h::t ->
           let (defs1, count1) = (anon_defs_from_func_decl (prefix, count) h) in
           let (defs2, count2) = (anon_defs_from_func_decl_list (prefix, count1) t) in
           (defs1@defs2, count2)

let anon_defs_from_tprogram tprog =
    let (defs, _) = (anon_defs_from_func_decl_list ("_", 0) (List.rev tprog.functions)) in
    List.rev defs

let rec print_anon_def anonDef = 
    Printf.printf "\n%s\n" (string_of_anon_def anonDef)

and print_anon_defs = function
      [] -> ()
    | [x] -> print_anon_def x
    | h::t -> print_anon_def h; print_anon_defs t

let print_symbol_table symtable =
   (Printf.printf "SYMBOL_TABLE:-------------\n\n");
    let l  = StringMap.bindings symtable in
         List.iter (fun (name, sym) -> Printf.printf "%s\n" name) l

