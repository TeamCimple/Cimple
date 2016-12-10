open Ast

module StringMap = Map.Make(String)

let rec string_of_type tp =
    let string_of_primitive_type = function
       | Void -> "Void"
       | Char -> "Char"
       | Short -> "Short"
       | Int -> "Int"
       | Long -> "Long"
       | Float -> "Float"
       | Double -> "Double"
       | Unsigned -> "Unsigned"
       | String -> "String"
    in match tp with
     PrimitiveType(t) -> string_of_primitive_type t
   | CustomType(s) -> "CustomType(" ^ s ^ ")"
   | AnonFuncType(t, tlist) -> "AnonFuncType(ReturnType: " ^ string_of_type t ^ ", ParamTypes: " ^ string_of_type_list tlist ^ ")"

and string_of_type_list = function
    [] -> ""
  | [x] -> string_of_type x
  | h::t -> string_of_type h ^ " " ^  string_of_type_list t

let var_name_from_direct_declarator = function
     DirectDeclarator(Var(Identifier(s))) -> s
   | PointerDirDecl(_, Var(Identifier(s))) -> s
   | _ -> raise(Failure("Pointers not yet supported"))

let var_name_from_declaration = function 
     Declaration(_ , InitDeclarator(dd)) -> var_name_from_direct_declarator dd 
   | Declaration(_, InitDeclaratorAsn(dd, _, _)) -> var_name_from_direct_declarator dd
   | Declaration(_, InitDeclList([InitDeclarator(dd)]))  -> var_name_from_direct_declarator dd
   | Declaration(_, InitDeclList([InitDeclaratorAsn(dd, _, _)])) ->
                   var_name_from_direct_declarator dd
   | _ -> raise(Failure("var_name_from_declaration: not yet supported")) 

let var_name_from_anon_decl adecl = match adecl.anon_decl_name with
    Identifier(s) -> s

let var_name_from_func_param = function
    FuncParamsDeclared(_, x) -> var_name_from_direct_declarator x
   | AnonFuncDecl(adecl) -> var_name_from_anon_decl adecl 
   | _ -> raise(Failure("pointers not supported"))

let rec type_from_declaration_specifiers = function
   DeclSpecTypeSpec(tspec) -> PrimitiveType(tspec)
 | DeclSpecTypeSpecAny(t) -> t
 | _ -> raise(Failure("type_from_declaration_specifiers: invalid specifiers"))
 (*| DeclSpecTypeSpecInitList(t, tDeclSpecs) -> CompoundType(t, type_from_declaration_specifiers tDeclSpecs)*)

let rec get_num_pointers ptrs = match ptrs with 
    | PtrType(ptr1, ptr2) -> (get_num_pointers ptr1) + (get_num_pointers ptr2)
    | Pointer -> 1

let type_from_declaration = function
        Declaration(decl_spec, InitDeclarator(PointerDirDecl(ptr, _))) ->
                PointerType(type_from_declaration_specifiers decl_spec,
                get_num_pointers ptr)
      | Declaration(decl_spec, InitDeclaratorAsn(PointerDirDecl(ptr, _), 
      _, _)) -> PointerType(type_from_declaration_specifiers decl_spec,
      get_num_pointers ptr)
      | Declaration(decl_spec,
      InitDeclList([InitDeclarator(PointerDirDecl(ptr, _))])) ->
              PointerType(type_from_declaration_specifiers decl_spec,
              get_num_pointers ptr)
      | Declaration(decl_spec,
      InitDeclList([InitDeclaratorAsn(PointerDirDecl(ptr, _), _, _)])) -> 
              PointerType(type_from_declaration_specifiers decl_spec,
              get_num_pointers ptr)
      | Declaration(decl_spec, _) -> type_from_declaration_specifiers decl_spec


let is_assignment_declaration decl = match decl with
      | Declaration(decl_spec,
      InitDeclList([InitDeclaratorAsn(_, _, _)])) -> true
      | Declaration(decl_spec, InitDeclaratorAsn(PointerDirDecl(ptr, _), _ , _))
      -> true
      | _ -> false
 

let rec type_from_func_param = function
     FuncParamsDeclared(t, _) -> type_from_declaration_specifiers t
   | ParamDeclWithType(declspecs) -> type_from_declaration_specifiers declspecs
   | AnonFuncDecl(adecl) -> type_from_anon_decl adecl 
   | _ -> raise(Failure("Only supports declared parameters"))

and type_list_from_func_param_list l = List.map type_from_func_param l

and param_list_has_void params func_name = 
        List.map (fun param -> if ((type_from_func_param param) = PrimitiveType(Void))
                               then 
                                raise(Failure("Using void as a function
                                        parameter for function: " ^ func_name))
                                else ()) params
    
and type_from_anon_decl d = AnonFuncType(d.anon_decl_return_type, type_list_from_func_param_list d.anon_decl_params)
  
let type_from_anon_def d = AnonFuncType(d.anon_return_type, type_list_from_func_param_list d.anon_params)

let symbol_from_func_param p = match p with
   | FuncParamsDeclared(decl_specs, decl) -> VarSymbol(var_name_from_direct_declarator decl, type_from_declaration_specifiers decl_specs)
   | AnonFuncDecl(d) -> AnonFuncSymbol(Astutil.string_of_identifier d.anon_decl_name, type_from_anon_decl d) 
   | _ -> raise(Failure("symbol_from_func_param not fully implemented. Cannot handle declarations with parameters as types alone"))

let symbol_from_declaration decl = match decl with  
    Declaration(declspec, _) -> VarSymbol(var_name_from_declaration decl,
    type_from_declaration decl)
   | _ -> raise(Failure("symbol_from_declaration: Unrecognized declaration"))    

let symbol_from_fdecl fdecl = FuncSymbol(var_name_from_direct_declarator
        fdecl.func_name, fdecl)

let symbol_from_struct struct_decl = StructSymbol(struct_decl.struct_name,
struct_decl)

let symbols_from_structs struct_decls = List.map symbol_from_struct struct_decls

let symbol_from_interface interface = InterfaceSymbol(interface.name, interface)

let symbols_from_interfaces interfaces = List.map symbol_from_interface
interfaces

let lookup_symbol_by_id symbols id = try StringMap.find 
(Astutil.string_of_identifier id) symbols with
        Not_found -> raise(Failure("undeclared identifier: " ^
        Astutil.string_of_identifier id))

let func_decl_from_anon_def anonDef = {
    return_type = DeclSpecTypeSpecAny(anonDef.anon_return_type);
    func_name = DirectDeclarator(Var(Identifier("")));
    receiver = ("", "");
    params = anonDef.anon_params;
    body = anonDef.anon_body
}
let type_from_identifier symbols id = 
        let x = lookup_symbol_by_id symbols id in match x with 
        | VarSymbol(_, t) -> t
        | FuncSymbol(_, func) -> type_from_declaration_specifiers
        func.return_type
        | StructSymbol(_, struct_decl) -> CustomType(struct_decl.struct_name)
        | AnonFuncSymbol(_, t) -> t 

let get_parameter_list symbol = match symbol with
        FuncSymbol(_, func) -> type_list_from_func_param_list func.params
      | AnonFuncSymbol(_, t) -> (match t with 
              AnonFuncType(_, tlist) -> tlist
            | _ -> raise(Failure("get_parameter_list: Error, invalid type for Anonymous function")))
      | _ -> raise(Failure("Shouldn't get parameter list for Var Symbol"))

let get_struct symbol = match symbol with 
        StructSymbol(_, struct_) -> struct_

let get_func symbol = match symbol with 
        FuncSymbol(_, func) -> func

let type_from_mem_access id t symbols  = 
        let sym = lookup_symbol_by_id symbols id in

        match sym with 
        VarSymbol(_, type_) ->

                ( match type_ with 

                | CustomType(cust_type) -> 

                        let cust_symb = lookup_symbol_by_id symbols
                        (Identifier(cust_type)) in
                        
                        ( match cust_symb with 
                        | StructSymbol(_, struct_decl) -> 

                                let list_decl = List.map var_name_from_declaration
                                struct_decl.members in 

                                if (List.mem t list_decl) then 
                                        let dec = List.find (fun decl ->
                                                (var_name_from_declaration decl)
                                                = t)
                                                struct_decl.members in  (
                                                        match (dec) with
                                                                | Declaration(decl_spec, _) ->
                                                                        type_from_declaration_specifiers
                                                                        decl_spec
                                                                | _ -> raise(Failure("Unhandled
                                                                        declaration")))
                               else
                                       raise(Failure("Invalid member of
                                       struct"))
                        )
                 | _ -> raise(Failure("Non Custom type trying to access
                 member"))
                )

       | _ -> raise(Failure("Member Access of non Struct"))

let rec type_from_expr symbols expr = match expr with
   Literal(_) -> PrimitiveType(Int)
  | FloatLiteral(_) -> PrimitiveType(Float)
  | StringLiteral(_) -> PrimitiveType(String)
  | Unop(e, _) -> type_from_expr symbols e
  | Binop(e1, _, _) -> type_from_expr symbols e1
  | Make(typ_, _) -> (match typ_ with 
                        | PointerType(base_type, count) ->
                                        PointerType(base_type, count + 1)
                        | _ -> PointerType(typ_, 1))
  | Call(_, Id(s), _) -> type_from_identifier symbols s
  | MemAccess(s, Identifier(t)) -> type_from_mem_access s t symbols 
  | Id(id) -> type_from_identifier symbols id
  | AsnExpr(expr, _, _) -> type_from_expr symbols expr
  | Pointify(e) -> (let typ_ = type_from_expr symbols e in 
                                match typ_ with 
                                | PointerType(base_type, count) ->
                                                PointerType(base_type, count+1)
                                                (* Because this isn't recursive
                                                 * we need to check if the
                                                 * existing type is a pointer
                                                 * and then add count to the
                                                 * existing pointer type *)
                                | _ -> PointerType(typ_, 1))
  | AnonFuncDef(adef) -> type_from_anon_def adef
  | Noexpr -> PrimitiveType(Void)

let rec check_compatible_anon_types t1 t2 =
        let f a b =
            let error_str = "t1 = " ^ string_of_type t1 ^ ", t2 = " ^ string_of_type t2
            in
            if a = b then () else raise(Failure("check_compatible_anon_types: Error, param types not equal: " ^ error_str))  in
        let check_lists_are_equal l1 l2 = List.iter2 f l1 l2 in
        match (t1, t2) with
           (AnonFuncType(rType1, plist1), AnonFuncType(rType2, plist2)) -> check_compatible_types rType1 rType2;
                                                                        check_lists_are_equal plist1 plist2
         | (_, _) -> raise(Failure("check_compatible_anon_types: Error, invalid anon types passed as arguments"))
                                                

and check_compatible_types t1 t2 = match (t1, t2) with
       (PrimitiveType(pt1), PrimitiveType(pt2)) -> (match pt1, pt2 with 
       | Void, Void -> ()
       | Int, Float -> raise(Failure("assigning float to int"))
       | Float, Int -> raise(Failure("assigning int to float"))
       | String, Float -> raise(Failure("assigning float to string"))
       | String, Int -> raise(Failure("assigning int to string"))
       | Int, String -> raise(Failure("assigning string to int"))
       | Float, String -> raise(Failure("assigning string to float"))
       | Void, Void -> ()
       | Int, Int -> ()
       | Float, Float -> ()
       | String, String -> ()
       | _ -> raise(Failure("Incompatible types")))
  | (PointerType(typ1_, c1), PointerType(typ2_, c2)) ->
                       ignore(check_compatible_types typ1_ typ2_);
                       if (c1 = c2) then () else raise(Failure("Incompatible
                       pointer depths " ^ (string_of_int c1) ^ " " ^
                       (string_of_int c2))) 
  | (PrimitiveType(_), CustomType(_)) -> raise(Failure("Primitive type
  incompatible with custom type"))
  | (CustomType(_), PrimitiveType(_)) -> raise(Failure("Custom type incompatible
  with primitive type"))
  | (CustomType(_), CustomType(_)) -> ()
  | AnonFuncType(_, _), AnonFuncType(_, _) -> check_compatible_anon_types t1 t2
  | _ -> raise(Failure("check_compatible_types: CompoundType not yet
  supported"))

let receiver_has_func receiver symbols func =
        let receiver_symbol = (lookup_symbol_by_id symbols (Identifier(receiver))) in
        let rec has_func symbol func = match symbol with
                | StructSymbol(type_, struct_) -> (match func.receiver with
                               (type2_, id) -> if (type2_ == type_) then () else
                                       if (struct_.extends != "") then ( let
                                       parent = lookup_symbol_by_id symbols
                                       (Identifier(struct_.extends)) in has_func parent func)
                                       else raise(Failure("method does not have
                                       receiver")))
                | InterfaceSymbol(type_, interface) -> (if (List.mem func
                interface.funcs) then () else raise(Failure("method isn't part
                of interface")))
        in 

        has_func receiver_symbol func

let check_constructor symbols struct_name params = 
        let struct_symbol = lookup_symbol_by_id symbols
        (Identifier(struct_name)) in 

        match struct_symbol with 

        StructSymbol(typ_, struct_) ->
               let constructor = struct_.constructor in 
               let param_list = constructor.constructor_params in
        

               if List.length param_list != List.length params then
                    if (constructor.constructor_name = "") then 
                        raise(Failure("Parameters for constructor not defined"))
               else
                    List.iter2 check_compatible_types
                    (type_list_from_func_param_list param_list)
                         (List.map (type_from_expr symbols) params)

  
       | _ -> raise(Failure("not handled\n"))
                
               
let rec check_compatible_type_lists tl1 tl2 = match (tl1, tl2) with
    ([], []) -> ()
  | ([x], [y]) -> check_compatible_types x y
  | (h1::t1, h2::t2) -> check_compatible_types h1 h2;
                        check_compatible_type_lists t1 t2
  | _ -> raise(Failure("check_compatible_type_lists: type lists are incompatible"))

let rec check_expr symbols e = match e with
     Id(Identifier(name)) -> if (StringMap.mem name symbols) == false then
                                raise(Failure("Undeclared identifier"))
                             else ()

  | Binop(e1, _, e2) -> let t1 = type_from_expr symbols e1 in
                         let t2 = type_from_expr symbols e2 in 
                         check_compatible_types t1 t2
   | Pointify(e) -> ()

   | Call(receiver, Id(id), expr_list) -> ignore (type_from_identifier symbols id);                      
                        ignore (let func_symbol = lookup_symbol_by_id symbols id in 
                            (match func_symbol with
                              FuncSymbol(_, _) -> 
                                    let func = get_func func_symbol in 
                                    if (receiver <> "") then (receiver_has_func receiver symbols func) 
                                    else ();
                                    let paramList = get_parameter_list (lookup_symbol_by_id symbols id) in               
                                    let exprList = List.map (type_from_expr symbols) expr_list in
                                    if List.length paramList != List.length exprList then 
                                            raise(Failure("Parameter count mismatch"))
                                    else 
                                        List.iter2 check_compatible_types paramList
                                        (List.map (type_from_expr symbols) expr_list)
                           | AnonFuncSymbol(s, AnonFuncType(returnType, paramTypes)) ->
                                   let passedParamTypes = List.map (type_from_expr symbols) expr_list in
                                   check_compatible_type_lists paramTypes passedParamTypes))


   | Unop(e, unop) -> check_expr symbols e;
                      let te = type_from_expr symbols e in 
                      (match (te, unop) with
                          (PrimitiveType(Void), _) -> raise(Failure("Cannot apply unary operator to void type"))
                        | (PrimitiveType(_), _) -> ()
                        | _ -> raise(Failure("Type/Unary Operator mismatch")))
   | MemAccess(s, Identifier(t)) -> ignore (type_from_mem_access s t symbols);
   | Make(typ_, expr_list) -> (match (typ_, expr_list)  with
                         | (PrimitiveType(s), [a]) -> ()
                         | (CustomType(s), e) -> (check_constructor symbols s e)
                         | _ -> raise(Failure("Invalid make")))
   | AsnExpr(expr, asnOp, e) -> 
                              let t1 = type_from_expr symbols e in
                              let t2 = type_from_expr symbols expr in
                              (match (t1, t2) with
                                 (PrimitiveType(Void), _) | (_, PrimitiveType(Void)) -> raise(Failure("Cannot assign to type void"))
                               | (PrimitiveType(_), CustomType(_)) -> raise(Failure("Cannot assign a struct to a primitive type"))
                               | (CustomType(_), PrimitiveType(_)) -> raise(Failure("Cannot assign a primitive type to a struct"))
                               | _ -> check_compatible_types t1 t2)

let symbols_from_decls decls = List.map symbol_from_declaration decls

let symbols_from_func_params func_params = List.map symbol_from_func_param func_params

let symbol_from_receiver receiver = match receiver with 
     | (type_, id) -> VarSymbol(id, CustomType(type_))

let type_from_receiver receiver = match receiver with 
     | (typ_, _) -> typ_

let compare_func_params p1 p2 = 
        let p1_types = List.map type_from_func_param p1 in
        let p2_types = List.map type_from_func_param p2 in
        List.iter2 check_compatible_types p1_types p2_types

let compare_func_names n1 n2 = 
        let n1_name = var_name_from_direct_declarator n1 in
        let n2_name = var_name_from_direct_declarator n2 in
        if (n1_name = n2_name) then () else raise(Failure("Function
        Names do not match"))

let compare_func_return_types r1 r2 = 
       check_compatible_types (type_from_declaration_specifiers r1)
  (type_from_declaration_specifiers r2)

let compare_functions f1 f2 = 
        let _ = compare_func_names f1.func_name f2.func_name in
        let _ = compare_func_params f1.params f2.params in
        compare_func_return_types f1.return_type f2.return_type

let symbols_from_fdecls fdecls = List.map symbol_from_fdecl fdecls

let get_id_from_symbol = function
     VarSymbol(id, _) -> id
   | FuncSymbol(id, _) -> id
   | StructSymbol(id, _) -> id
   | AnonFuncSymbol(id, t) -> id

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
                | FuncSymbol(_, func) -> let t1 =
type_from_declaration_specifiers func.return_type in let
                        t2 =  type_from_expr symbols expr
                in check_compatible_types t1 t2)
    | Declaration(declspec, InitDeclList([InitDeclarator(decl)])) -> ()               
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
  | Break -> ()

(*let rec check_statement_list func symbol_table stmtList = match stmtList with *)
    (*[] -> ()*)
  (*| [x] -> check_statement func symbol_table x*)
  (*| h::t -> check_statement func symbol_table h;*)
            (*check_statement_list func symbol_table t*)

let func_decl_from_anon_func_def anonDef = {
    return_type = DeclSpecTypeSpecAny(anonDef.anon_return_type);
    func_name = DirectDeclarator(Var(Identifier("")));
    receiver = ("", "");
    params = anonDef.anon_params;
    body = anonDef.anon_body
}

let rec check_anon_func_def symbol_table anonDef =  
    check_statement (func_decl_from_anon_func_def anonDef) symbol_table anonDef.anon_body

(* This function checks 1) Is there a cycle in the inheritence tree and 2)
 * checks that all extensions are valid i.e. no extending oneself or a non
 * existenct struct *)

let validate_all_inheritence symbols structs =  
        let validate_inheritence symbols strct =
                (* It could be that the struct inside of the symbols map is
                 * different from the struct we pass into this function is not
                 * the same as the one in our symbol table since the symbol
                 * table could be modified.
                 * *)
                let StructSymbol(_, struct_) = StringMap.find strct.struct_name symbols in
                (* Check to see if the parent struct is defined*) 
                if (StringMap.mem struct_.extends symbols) 
                then 
                        let StructSymbol(name, parent_struct) = StringMap.find struct_.extends
                        symbols in 

                        (* Check if parent struct is a member of struct_'s
                         * children list. If it is then we have circular
                         * definition *)

                        if (List.mem parent_struct.struct_name struct_.children)
                        then 
                                raise(Failure("Circular inheritence: " ^
                                parent_struct.struct_name ^ " extends " ^
                                struct_.struct_name ^ " but is also a parent of
                                " ^ struct_.struct_name))
                       else
                               (* We found our parent struct and are about to
                                * add ourselves and our children to its children
                                * list. Sanity check we aren't extending
                                * ourselves
                                *  *)
                               if (struct_.struct_name = name) 
                               
                               then
                                       raise(Failure("Struct: " ^ struct_.struct_name ^"
                                       cannot extend itself"))
                               else
                                       let list_to_add = struct_.children @
                                         [struct_.struct_name] in
                                
                                        let updated_struct = {
                                                members = parent_struct.members;
                                                struct_name = parent_struct.struct_name;
                                                extends = parent_struct.extends;
                                                methods = []; 
                                                implements = parent_struct.implements;
                                                children = (parent_struct.children @ list_to_add);
                                                constructor = parent_struct.constructor;
                                        } in
                               
                                        let new_symbol = StructSymbol(name,
                                        updated_struct) in 

                                        StringMap.add name
                                        new_symbol symbols

                else 
                        if (struct_.extends = "") then symbols else
                                raise(Failure("extending a struct that isn't
                                defined: " ^ struct_.extends))
                in 
                
        List.fold_left validate_inheritence symbols structs

(* Assumes that the symbol table has been validated for 
 * inheritence rules and duplicate entries *)

let rec get_parents symbols struct_ = 
        if (struct_.extends <> "")
        
        then 
                let StructSymbol(_, parent_struct) = StringMap.find
                struct_.extends symbols in 

                [parent_struct] @ (get_parents symbols parent_struct) 
       else 
                []
(* This function gets the constructor of the closest
 * ancestor of struct_.  *)
let rec get_ancestors_constructor symbols struct_ =
        if (struct_.extends = "")
        
        then struct_.constructor

        else
                let StructSymbol(_, parent_struct) = StringMap.find struct_.extends symbols in
                        if (parent_struct.constructor.constructor_name = "")
                                then get_ancestors_constructor symbols parent_struct
                        else 
                                parent_struct.constructor

let check_void_decl decl = match decl with 
        | Declaration(decl_spec, _) -> 
                        if (type_from_declaration_specifiers decl_spec =
                                PrimitiveType(Void)) then
                                        raise(Failure("Invalid Declaration of
type Void. Trying to declare variable: " ^ var_name_from_declaration decl ^ " as
void"))
                        else
                                ()
        | _ -> raise(Failure("Unhandled Declaration"))
        
        

let update_fields functions symbols structs  = 
        
        
        let _ = validate_all_inheritence symbols structs in

        let update_field functions symbols strct =
                let StructSymbol(_, struct_) = StringMap.find strct.struct_name symbols in
                let strct_methods = List.filter (fun func -> if
                        (type_from_receiver func.receiver = strct.struct_name)
                        then true else 
                                false) functions in 
                if (struct_.extends = "")
                then
                        let updated_child_struct = {
                                struct_name = struct_.struct_name;
                                members = struct_.members;
                                children = struct_.children;
                                methods = strct_methods;
                                constructor = struct_.constructor;
                                implements = struct_.implements;
                                extends = struct_.extends;
                        } in 
                        StringMap.add struct_.struct_name
                        (StructSymbol(struct_.struct_name,
                        updated_child_struct)) symbols
                else 
                        let parents = get_parents symbols struct_ in 
                        match parents with 

                        | [] -> symbols
                        | _ -> List.fold_left (fun sym parent_struct -> 
                                                let updated_child_struct = {
                                                        struct_name =
                                                                struct_.struct_name;
                                                        members =
                                                                struct_.members
                                                                @
                                                                parent_struct.members;
                                                        children =
                                                                struct_.children;
                                                        methods = strct_methods;
                                                        constructor = (if
                                                                (struct_.constructor.constructor_name
                                                                = "") then
                                                                        (get_ancestors_constructor
                                                                        sym
                                                                        struct_)
                                                                      else 
                                                                        struct_.constructor);
                                                        implements =
                                                                struct_.implements;
                                                        extends =
                                                                struct_.extends;
                                                } in 

                                                StringMap.add
                                                struct_.struct_name
                                                (StructSymbol(struct_.struct_name,
                                                updated_child_struct))
                                                sym) symbols parents in
                                                                       
               List.fold_left (fun symbols struct_ -> (update_field functions
               symbols struct_)) symbols structs

(* Updates structs in the program object with the ones populated in symbol table *)
let update_structs_in_program program  =
        let fdecls = program.functions @ [{
               return_type = DeclSpecTypeSpec(Int); 
               func_name = DirectDeclarator(Var(Identifier("printf")));
               params = [FuncParamsDeclared(DeclSpecTypeSpec(String),
               DirectDeclarator(Var(Identifier("x"))))];
               receiver = ("", "");
               body = CompoundStatement([], []);                                          
       }] in

       let symbol_table = update_fields program.functions (List.fold_left (fun m symbol -> if StringMap.mem
                (get_id_from_symbol symbol) m then
                        raise(Failure("redefining identifier")) else StringMap.add
                        (get_id_from_symbol symbol) symbol m) StringMap.empty
                         (symbols_from_decls program.globals @
                         symbols_from_fdecls (List.filter (fun func -> if
                                 (type_from_receiver func.receiver = "") then true
                                 else false) fdecls) @ (symbols_from_structs program.structs)
                         @ (symbols_from_interfaces program.interfaces)))
                        program.structs in
         

        let rec get_structs_from_symbols structs symbol_table = 
                match structs with 
                | [] -> []
                | h::t -> (let StructSymbol(_, struct_) = StringMap.find
                h.struct_name symbol_table in [struct_] @ get_structs_from_symbols t
                symbol_table) 
        in 

        let structs_ = get_structs_from_symbols program.structs symbol_table in 
        
        {
                globals = program.globals;
                structs = structs_;
                interfaces = program.interfaces;
                functions = program.functions;
        }

let check_struct_fields struct_ = 
         ignore (List.map check_void_decl struct_.members);

         List.fold_left (fun sym decl -> if
        (StringMap.mem (var_name_from_declaration decl)
        sym) then raise(Failure("Struct field: " ^
        (var_name_from_declaration (decl)) ^ " was redeclared")) else
                StringMap.add (var_name_from_declaration decl) decl sym)
        StringMap.empty struct_.members 

let convert_constructor_to_fdecl constructor = 
        {
                return_type = DeclSpecTypeSpecAny(PrimitiveType(Void));
                func_name =
                        DirectDeclarator(Var(Identifier(constructor.constructor_name)));
                body = constructor.constructor_body;
                params = constructor.constructor_params;
                receiver = ("", "");
        }

let check_constructor_definition_in_struct struct_ = 
        ignore (List.iter (fun decl -> if (is_assignment_declaration decl) then
                raise(Failure("Cannot have assignment declaration in struct"))
        else ()) struct_.members);

        let constructor = struct_.constructor in
        let symbols =  List.fold_left (fun symbol_table sym -> if
        (StringMap.mem (get_id_from_symbol sym)
        symbol_table) then raise(Failure("Struct field: " ^
        (get_id_from_symbol sym) ^ " was redeclared")) else
                StringMap.add (get_id_from_symbol sym) sym symbol_table) 
        StringMap.empty (symbols_from_decls struct_.members @
        symbols_from_func_params constructor.constructor_params) in 

        let func = convert_constructor_to_fdecl constructor in 
        if (constructor.constructor_name = "") then () else
                check_statement func
        symbols constructor.constructor_body


let get_method_names struct_ = List.map (fun func -> var_name_from_direct_declarator func.func_name) struct_.methods


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

       ignore (List.map check_void_decl program.globals);

       let fnames =  (List.map (fun func -> var_name_from_direct_declarator func.func_name)
                                (List.filter (fun func -> if (type_from_receiver
                                func.receiver = "") then true else false) program.functions)) in
                                report_duplicate (fun a -> "duplicate functions:
                                        " ^ a) (fnames);

       let program = update_structs_in_program program 
                       in ignore(List.map check_struct_fields program.structs);

        ignore (List.map check_constructor_definition_in_struct
        program.structs);

       let struct_names = List.map (fun struct_ -> struct_.struct_name)
       program.structs in 

       report_duplicate (fun a -> "duplicate structs: " ^ a) (struct_names);

       let check_duplicate_struct struct_ = 
               report_duplicate (fun a -> "duplicate method: " ^ a)
       (get_method_names struct_) in
       
       List.map check_duplicate_struct (program.structs);

       let has_main = List.mem "main" fnames in
       if has_main then () else raise(Failure("no function main declared")); 

       let is_printf_redefined = List.mem "printf" fnames in
       if is_printf_redefined then raise(Failure("cannot redefine printf")) else
               ();
     
       
       let fdecls = (List.filter (fun func -> if (type_from_receiver
       func.receiver = "") then true else false) program.functions) @ [{
               return_type = DeclSpecTypeSpec(Int); 
               func_name = DirectDeclarator(Var(Identifier("printf")));
               params = [FuncParamsDeclared(DeclSpecTypeSpec(String),
               DirectDeclarator(Var(Identifier("x"))))];
               receiver = ("", "");
               body = CompoundStatement([], []);                                          
       }] in

       (* Build map of function declarations *)
       let functions_map = List.fold_left (fun m func -> StringMap.add
       (var_name_from_direct_declarator func.func_name) func m) StringMap.empty
       fdecls in

       let check_function func =
                let func_params = List.map var_name_from_func_param func.params
                in

                report_duplicate (fun a -> "duplicate function parameters: " ^
                a) func_params;

                param_list_has_void func.params (var_name_from_direct_declarator
                func.func_name);

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
                         symbols_from_fdecls fdecls @
                         (symbols_from_func_params func.params) @ (symbols_from_decls
                         (get_decls_from_compound_stmt func.body))
                         @ (symbols_from_structs program.structs)
                         @ (symbols_from_interfaces program.interfaces)
                         @ ([symbol_from_receiver func.receiver]))

                in
                
                List.iter (check_local_declaration symbol_table)
                (get_decls_from_compound_stmt func.body);

                List.iter (check_statement func symbol_table)
                (get_stmts_from_compound_stmt func.body); 
                        
        in List.iter check_function program.functions;
