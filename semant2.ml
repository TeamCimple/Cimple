open Ast

module StringMap = Map.Make(String)

let var_name_from_direct_declarator = function
     DirectDeclarator(Var(Identifier(s))) -> s
   | _ -> raise(Failure("Pointers not yet supported"))

let var_name_from_declaration = function 
     Declaration(_ , InitDeclarator(dd)) -> var_name_from_direct_declarator dd 
   | Declaration(_, InitDeclaratorAsn(dd, _, _)) -> var_name_from_direct_declarator dd
   | Declaration(_, InitDeclList([InitDeclarator(dd)]))  -> var_name_from_direct_declarator dd
   | Declaration(_, InitDeclList([InitDeclaratorAsn(dd, _, _)])) ->
                   var_name_from_direct_declarator dd
   | _ -> raise(Failure("var_name_from_declaration: not yet supported")) 

let var_name_from_func_param = function
    FuncParamsDeclared(_, x) -> var_name_from_direct_declarator x 
   | _ -> raise(Failure("pointers not supported"))

let rec type_from_declaration_specifiers = function
   DeclSpecTypeSpec(tspec) -> PrimitiveType(tspec)
 | DeclSpecTypeSpecAny(t) -> t
 | DeclSpecTypeSpecInitList(t, tDeclSpecs) -> CompoundType(t, type_from_declaration_specifiers tDeclSpecs)

let type_from_func_param = function
    FuncParamsDeclared(t, _) -> type_from_declaration_specifiers t
   | _ -> raise(Failure("Only supports declared parameters"))

let symbol_from_func_param p = match p with
   | FuncParamsDeclared(decl_specs, decl) ->
                   VarSymbol(var_name_from_direct_declarator decl,
                   type_from_declaration_specifiers decl_specs)
   | _ -> raise(Failure("symbol_from_func_param not fully implemented. Cannot
   handle declarations with parameters as types alone"))

let symbol_from_declaration decl = match decl with  
   | Declaration(declspec, _) -> 
          VarSymbol(var_name_from_declaration decl, type_from_declaration_specifiers
     declspec)
   | _ -> raise(Failure("symbol_from_declaration: Unrecognized declaration"))    

let symbol_from_fdecl fdecl = FuncSymbol(var_name_from_direct_declarator
        fdecl.func_name, type_from_declaration_specifiers fdecl.return_type,
        List.map type_from_func_param fdecl.params)

let lookup_symbol_by_id symbols id = try StringMap.find id symbols with
        Not_found -> raise(Failure("undeclared identifier: " ^ id))

let type_from_identifier symbols id = 
        let x = lookup_symbol_by_id symbols id in match x with 
        | VarSymbol(_, t) -> t
        | FuncSymbol(_, t, _) -> t

let get_parameter_list symbol = match symbol with 
        FuncSymbol(_, _, tlist) -> tlist
        | _ -> raise(Failure("Shouldn't get parameter list for Var Symbol"))

let rec type_from_expr symbols expr = match expr with
   Literal(_) -> PrimitiveType(Int)
  | FloatLiteral(_) -> PrimitiveType(Float)
  | Unop(e, _) -> type_from_expr symbols e
  | Binop(e1, _, _) -> type_from_expr symbols e1
  | Call(Identifier(id), _) -> type_from_identifier symbols id 
  | Id(Identifier(id)) -> type_from_identifier symbols id
  | AsnExpr(Identifier(id), _, _) -> type_from_identifier symbols id
  | Noexpr -> PrimitiveType(Void)


let check_compatible_types t1 t2 = match (t1, t2) with
       (PrimitiveType(pt1), PrimitiveType(pt2)) -> (match pt1, pt2 with 
       | Void, Void -> raise(Failure("Cannot assign to void type"))
       | Int, Float -> raise(Failure("assigning float to int"))
       | Float, Int -> raise(Failure("assigning int to float"))
       | Int, Int -> ()
       | Float, Float -> ())
  | (CustomType(_), CustomType(_)) -> ()
  | (StringType, StringType) -> ()
  | _ -> raise(Failure("check_compatible_types: CompoundType not yet
  supported"))

let rec check_expr symbols e = match e with
     Id(Identifier(name)) -> if (StringMap.mem name symbols) == false then
                                raise(Failure("Undeclared identifier"))
                             else ()

  | Binop(e1, _, e2) -> let t1 = type_from_expr symbols e1 in
                         let t2 = type_from_expr symbols e2 in 
                         check_compatible_types t1 t2

   | Call(Identifier(id), expr_list) -> ignore (type_from_identifier symbols id); 
                                        
                        let paramList = get_parameter_list (lookup_symbol_by_id symbols id) in 
                                       
                        let exprList = List.map (type_from_expr symbols) expr_list in

                        if List.length paramList != List.length exprList then 
                                raise(Failure("Parameter count mismatch"))
                        else 
                            List.iter2 check_compatible_types paramList
                            (List.map (type_from_expr symbols) expr_list)

   | Unop(e, unop) -> check_expr symbols e;
                      let te = type_from_expr symbols e in 
                      (match (te, unop) with
                          (PrimitiveType(Void), _) -> raise(Failure("Cannot apply unary operator to void type"))
                        | (PrimitiveType(_), _) -> ()
                        | _ -> raise(Failure("Type/Unary Operator mismatch")))
   | AsnExpr(Identifier(id), asnOp, e) -> 
                              let t1 = type_from_expr symbols e in
                              let t2 = type_from_identifier symbols id in
                              (match (t1, t2) with
                                 (PrimitiveType(Void), _) | (_, PrimitiveType(Void)) -> raise(Failure("Cannot assign to type void"))
                               | (PrimitiveType(_), CustomType(_)) -> raise(Failure("Cannot assign a struct to a primitive type"))
                               | (CustomType(_), PrimitiveType(_)) -> raise(Failure("Cannot assign a primitive type to a struct"))
                               | (StringType, (PrimitiveType(_) | CustomType(_))) -> raise(Failure("Cannot assign a non-string to a string"))
                               | _ -> ())

let symbols_from_decls decls = List.map symbol_from_declaration decls

let symbols_from_func_params func_params = List.map symbol_from_func_param
func_params

let symbols_from_fdecls fdecls = List.map symbol_from_fdecl fdecls

let get_id_from_symbol = function
        VarSymbol(id, _) -> id
   | FuncSymbol(id, _, _) -> id

let get_decls_from_compound_stmt stmt = match stmt with 
        CompoundStatement(x, y) -> x
   | _ -> []

let get_stmts_from_compound_stmt stmt = match stmt with 
        CompoundStatement(x, y) -> y
   | _ -> []

let check_local_declaration symbols decl = match decl with
     Declaration(declspec, InitDeclList([InitDeclaratorAsn(declarator, asnOp,
     expr)])) ->

             let sym = symbol_from_declaration decl in 
             (match sym with
                 VarSymbol(_, t1) -> let t2 = type_from_expr symbols expr in
                                          check_compatible_types t1 t2
                | FuncSymbol(_, t1, _) -> let t2 = type_from_expr symbols expr
                in check_compatible_types t1 t2)

    | _ -> raise(Failure("check_local_declaration not supported"))


let check_bool_expr symbols expr = check_compatible_types (type_from_expr symbols
expr) (PrimitiveType(Int))


let add_to_symbol_table tbl decls = List.fold_left (fun m symbol -> if StringMap.mem
       (get_id_from_symbol symbol) m then
       raise(Failure("redefining variable: " ^
               get_id_from_symbol symbol)) else StringMap.add
       (get_id_from_symbol symbol) symbol m) tbl
       (symbols_from_decls decls)
 
let rec check_statement func symbol_table stmt = match stmt with
     Expr(e) -> check_expr symbol_table e
  | Return(e) -> check_compatible_types (type_from_expr symbol_table e)
  (type_from_declaration_specifiers func.return_type)
  | If(e, s1, s2) -> check_bool_expr symbol_table e; check_statement
  func symbol_table s1;  check_statement func symbol_table s2
  | EmptyElse -> ()
  | For(_, e2, _, st) -> check_bool_expr symbol_table e2; check_statement
  func symbol_table st
  | While(e, s) -> check_bool_expr symbol_table e; check_statement func symbol_table
  s
  | CompoundStatement(dl, sl) -> let tbl = add_to_symbol_table symbol_table dl
  in List.iter (check_local_declaration tbl) dl; List.iter (check_statement func tbl) sl


let check_program program =
        let sdecls = List.map var_name_from_declaration program.globals in
        let report_duplicate exceptf list =
                let rec helper = function
	                n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
                        | _ :: t -> helper t
                        | [] -> ()
                in helper (List.sort compare list)
        in 
        
       report_duplicate (fun a -> "duplicate for variable: " ^ a)  (sdecls); 

       let fnames = List.map (fun func -> var_name_from_direct_declarator func.func_name) program.functions in
       report_duplicate (fun a -> "duplicate functions: " ^ a) (fnames);

       let has_main = List.mem "main" fnames in
       if has_main then () else raise(Failure("no function main declared")); 

       (* Build map of function declarations *)
       let functions_map = List.fold_left (fun m func -> StringMap.add
       (var_name_from_direct_declarator func.func_name) func m) StringMap.empty
       program.functions in

        let check_function func =
                let func_params = List.map var_name_from_func_param func.params
                in

                report_duplicate (fun a -> "duplicate function parameters: " ^
                a) func_params;

                let local_decls = List.map var_name_from_declaration
                (get_decls_from_compound_stmt func.body) in

                report_duplicate (fun a -> "duplicate local variable: " ^ a)
                (local_decls);


               let symbol_table = List.fold_left (fun m symbol -> if StringMap.mem
                (get_id_from_symbol symbol) m then
                        raise(Failure("redefining variable: " ^
                        get_id_from_symbol symbol ^ " in function: " ^
                        (var_name_from_direct_declarator func.func_name))) else StringMap.add
                        (get_id_from_symbol symbol) symbol m) StringMap.empty
                         (symbols_from_decls program.globals @
                         symbols_from_fdecls program.functions @
                         (symbols_from_func_params func.params) @ (symbols_from_decls
                         (get_decls_from_compound_stmt func.body)))
                in
                
                List.iter (check_local_declaration symbol_table)
                (get_decls_from_compound_stmt func.body);

                List.iter (check_statement func symbol_table)
                (get_stmts_from_compound_stmt func.body); 
                        
        in List.iter check_function program.functions;

