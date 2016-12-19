open Ast

module StringMap = Map.Make(String)

let add_symbol_list_to_symtable symlist symtable = 
    List.fold_left (fun tbl x ->                     
                        if 
                            (StringMap.mem (Astutil.string_of_symbol_simple x) tbl) then raise(Failure("Error, redefining symbol"))
                        else
                            (StringMap.add (Astutil.string_of_symbol_simple x) x tbl)) symtable symlist
(*let merge_symtables s1 s2 = *)
    (*StringMap.Misc.union (fun key v1 v2 ->*)
        (*(match v2, v2 with*)
           (*x, y -> if x != y then *)
                    (*raise(Failure("concat_symtables: Error - duplicate symbol"))*)
                   (*else x*)
        (*| None, y -> y*)
        (*| x, None -> x)) s1 s2*)

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
    | NoPointer -> 0

let get_func_name fdecl = var_name_from_direct_declarator fdecl.func_name

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
     FuncParamsDeclared(t, PointerDirDecl(ptr, _)) ->
             PointerType(type_from_declaration_specifiers t, get_num_pointers
             ptr)
   | FuncParamsDeclared(t, _) -> type_from_declaration_specifiers t
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
   | FuncParamsDeclared(decl_specs, decl) ->
                   VarSymbol(var_name_from_direct_declarator decl,
                   type_from_func_param p)
   | AnonFuncDecl(d) -> AnonFuncSymbol(Astutil.string_of_identifier d.anon_decl_name, type_from_anon_decl d) 
   | _ -> raise(Failure("symbol_from_func_param not fully implemented. Cannot handle declarations with parameters as types alone"))

let symbol_from_declaration decl = match decl with  
    Declaration(declspec, _) -> VarSymbol(var_name_from_declaration decl,
    type_from_declaration decl)
   | _ -> raise(Failure("symbol_from_declaration: Unrecognized declaration"))    

let symbol_table_key_for_method struct_name func_name = let cstruct_name =
        String.concat "" ["_struct";struct_name] in String.concat "_"
[cstruct_name;func_name] 

let symbol_from_fdecl fdecl = 
        let func_name = var_name_from_direct_declarator fdecl.func_name
        in if (fdecl.receiver = ("", "")) then FuncSymbol(func_name, fdecl) else
                FuncSymbol((symbol_table_key_for_method (fst fdecl.receiver)
                func_name), fdecl)

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

let get_interface symbol_table name = 
        let sym = lookup_symbol_by_id symbol_table (Identifier(name)) in 
        match sym with 
        | InterfaceSymbol(_, interface) -> interface
        | _ -> raise(Failure("cannot find interface"))

let is_interface symbol_table name = 
        let sym = lookup_symbol_by_id symbol_table (Identifier(name)) in 
        match sym with 
        | InterfaceSymbol(_, interface) -> true
        | _ -> false

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
        | InterfaceSymbol(name, _) -> CustomType(name)
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

let get_type_from_struct_member cust_type symbols t = 
        let cust_symb = lookup_symbol_by_id symbols
                        (Identifier(cust_type)) in
                        
        match cust_symb with 
        | StructSymbol(_, struct_decl) -> 
              let list_decl = List.map var_name_from_declaration struct_decl.members in 
              if (List.mem t list_decl) then 
                  let dec = List.find (fun decl ->
                                       (var_name_from_declaration decl) = t)
                  struct_decl.members in  (type_from_declaration dec)
             else
                 raise(Failure("Invalid member of  struct"))
                       

let type_from_mem_access type_ t symbols  = 
        match type_ with 
                | CustomType(cust_type) -> get_type_from_struct_member cust_type
                symbols t

                | PointerType(CustomType(cust_type), 1) -> get_type_from_struct_member
                cust_type symbols t 
                | _ -> raise(Failure("Non Custom type trying to access
                 member"))

       | _ -> raise(Failure("Member Access of non Struct"))

let is_interface symbols id = 
        let sym = lookup_symbol_by_id symbols id in match sym with 
        | InterfaceSymbol(_, _) -> true
        | _ -> false

let rec t1_inherits_t2 t1 t2 symbols = 
        let sym1 = (lookup_symbol_by_id symbols (Identifier(t1))) in
                match sym1 with 
                | StructSymbol(type_, struct_) -> (let sym2 = (lookup_symbol_by_id
                         symbols (Identifier(t2))) in match sym2 with
                         | StructSymbol(type_2, struct2_) ->  
                                if (struct_.extends <> "") then 
                                        (if (struct_.extends <> struct2_.struct_name) 
                                
                                                then (t1_inherits_t2
                                                struct_.extends t2 symbols) 
                                                else false
                                         ) else (if (type_ = type_2) then true
                                else false)
                          | _ -> false)
                | _ -> false

let rec get_interface_for_struct t1 symbols = 
        let sym1 = (lookup_symbol_by_id symbols (Identifier(t1))) in 
                match sym1 with 
                | StructSymbol(typ_, struct_) -> (if (struct_.implements <> "") then 
                                struct_.implements else (if (struct_.extends <> "")
                                then get_interface_for_struct struct_.extends symbols else
                                        ""))
               | _ -> raise(Failure("Not supported"))



let rec t1_implements_t2 t1 t2 symbols =
        let sym1 = (lookup_symbol_by_id symbols (Identifier(t1))) in 
                match sym1 with 
                | StructSymbol(typ_, struct_) -> (let sym2 =
                        (lookup_symbol_by_id symbols (Identifier(t2))) in match
                        sym2 with 
                        | InterfaceSymbol(typ_, _) -> if (struct_.implements =
                                t2) then true else (if (struct_.extends <> "")
                                then t1_implements_t2 struct_.extends t2 symbols else
                                        false)
                        | _ -> raise(Failure("Not supported")))
               | _ -> raise(Failure("Not supported"))

let rec t1_inherits_t2 t1 t2 symbols = 
        let sym1 = (lookup_symbol_by_id symbols (Identifier(t1))) in 
                match sym1 with 
                | StructSymbol(typ_, struct_) -> (let sym2 =
                        (lookup_symbol_by_id symbols (Identifier(t2))) in match
                        sym2 with
                        | StructSymbol(typ2_, struct2_) -> if (struct_.extends
                        <> "") then (if (struct_.extends = struct2_.struct_name)
                        then true else t1_inherits_t2 struct_.extends t2 symbols) else
                                false)

let check_compatible_custom_types symbols t1 t2 =
        let t1_sym = lookup_symbol_by_id symbols t1 in 
        let t2_sym  = lookup_symbol_by_id symbols t2 in
        match (t1_sym, t2_sym) with 
        | (StructSymbol(t1_name, t1_struct), StructSymbol(t2_name, t2_struct))
        -> if (t1_name = t2_name) then () else (
                if (t1_inherits_t2 t2_name t1_name symbols) then () else
                raise(Failure("Incompatible types:" ^ t1_name ^ "," ^ t2_name)))
        | (StructSymbol(t1_name, t1_struct), InterfaceSymbol(name, _)) ->
                        raise(Failure("Incompatible types:" ^ t1_name ^ "," ^
                        name))
        | (InterfaceSymbol(name, _), StructSymbol(t2_name, t2_struct)) ->  if
                (t1_implements_t2 t2_name name symbols) then () else
                        raise(Failure("Incompatible types:" ^ t2_name ^ "," ^
                        name)) 

(* This is meant to check assignments of custom type to pointer type
 * The only case this is valid is if a is pointer and b is interface
 * which a satisfies *)

let check_pointer_and_custom_types a b symbols =
     let sym_a = lookup_symbol_by_id symbols (Identifier(a)) in 

     let sym_b = lookup_symbol_by_id symbols (Identifier(b)) in 

     match (sym_a, sym_b) with 
     | (StructSymbol(_, _), StructSymbol(_, _)) ->
             raise(Failure("Assigning:" ^ b ^"to" ^a ^ "which
                               is pointer type"))
     | (StructSymbol(strct, _), InterfaceSymbol(name, _)) -> if
                       (t1_implements_t2 strct name symbols) then () else
                               raise(Failure(strct ^ "does not implement" ^
                               name))
     | _ -> raise(Failure("Assigning incompatible custom types,
               pointer and non pointer"))


let rec check_compatible_anon_types symbols t1 t2 =
        let f a b =
            let error_str = "t1 = " ^ string_of_type t1 ^ ", t2 = " ^ string_of_type t2
            in
            if a = b then () else raise(Failure("check_compatible_anon_types: Error, param types not equal: " ^ error_str))  in
        let check_lists_are_equal l1 l2 = List.iter2 f l1 l2 in
        match (t1, t2) with
           (AnonFuncType(rType1, plist1), AnonFuncType(rType2, plist2)) ->
                   check_compatible_types symbols rType1 rType2;
                   check_lists_are_equal plist1 plist2
         | (_, _) -> raise(Failure("check_compatible_anon_types: Error, invalid anon types passed as arguments"))

and check_compatible_types symbols t1 t2 = match (t1, t2) with
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
                       ignore(check_compatible_types symbols typ1_ typ2_);
                       if (c1 = c2) then () else raise(Failure("Incompatible
                       pointer depths " ^ (string_of_int c1) ^ " " ^
                       (string_of_int c2))) 
  | (PrimitiveType(_), CustomType(_)) -> raise(Failure("Primitive type
  incompatible with custom type"))
  | (CustomType(_), PrimitiveType(_)) -> raise(Failure("Custom type incompatible
  with primitive type"))
  | (CustomType(a), CustomType(b)) -> check_compatible_custom_types symbols (Identifier(a))
  (Identifier(b)) 
  | (PointerType(CustomType(a), 1), CustomType(b)) ->
                  check_pointer_and_custom_types a b symbols
  | (CustomType(a), PointerType(CustomType(b), 1)) -> if (t1_implements_t2 b a
  symbols) then () else raise(Failure("Incompatible types, pointer and custom type: "
  ^ b ^ " " ^ a))
  | (PointerType(_, _), PrimitiveType(_)) -> raise(Failure("Cannot compare
  pointer and primitive"))
  | (PrimitiveType(_), PointerType(_, _)) -> raise(Failure("Cannot compare
  pointer and primitive"))
  | (PointerType(_, _), NilType) -> ()
  | (PrimitiveType(_), NilType) -> raise(Failure("Incompatible types: primitive
  and nil. "))
  | (CustomType(s), NilType) -> raise(Failure("Incompatible types: " ^ s ^ "
  and " ^ "nil")) 
  | AnonFuncType(_, _), AnonFuncType(_, _) -> check_compatible_anon_types
  symbols t1 t2
  | (PrimitiveType(_), AnonFuncType(rtype, _)) -> check_compatible_types symbols t1 rtype
  | _ -> 
          let t1Str = string_of_type t1 in
          let t2Str = string_of_type t2 in
          let errorStr = "check_compatible_types: Error - " ^ t1Str ^ " and " ^ t2Str ^ " not yet supported" in
          raise(Failure(errorStr))
  (*| _ -> raise(Failure("check_compatible_types: *)
  (*[>supported"))<]*)

let rec get_fdecl_for_receiver typ_ tSymbol_table func_name = 
        let object_symbol = (lookup_symbol_by_id tSymbol_table (Identifier(typ_))) in 
        let rec find_func symbol func_name = match symbol with 
                | StructSymbol(type_, struct_) -> (let key = symbol_table_key_for_method struct_.struct_name func_name in 
                                 if (StringMap.mem key tSymbol_table) then
                                        (let FuncSymbol(_, fdecl) = StringMap.find key
                                        tSymbol_table in fdecl)
                                 else (
                                         if (struct_.extends <> "") then
                                                 get_fdecl_for_receiver
                                                 struct_.extends
                                                 tSymbol_table func_name
                                         else
                                                raise(Failure("Receiver doesn't
                                                have function"))))
                | InterfaceSymbol(type_, interface) -> (if (List.exists (fun
                        fdecl -> if (get_func_name fdecl = func_name) then true else false)
                interface.funcs) then
                                (List.find (fun fdecl -> if(get_func_name fdecl =
                                        func_name) then true else false)
                                interface.funcs)
                                 else
                                        raise(Failure("Interface doesn't have
                                        function"))) in 
        find_func object_symbol func_name

let type_of_array_type symbols = function
        | ArrayType(type_of_array, pointer, expr) ->
                                                 
                        let num_pointers = get_num_pointers pointer in 
                        PointerType(type_of_array, num_pointers + 1) 
        | _ -> raise(Failure("type_of_array_type should not be called on non
        array_type"))

let rec get_array_access_depth depth expr = match expr with 
        | ArrayAccess(expr, _) -> 1 + (get_array_access_depth 1 expr)
        | _ -> 1

let rec type_from_array_access symbols expr = match expr with 
  | ArrayAccess(e1, e2) -> type_from_array_access symbols e1
  | _ -> type_from_expr symbols expr 

and type_from_expr symbols expr = match expr with
   Literal(_) -> PrimitiveType(Int)
  | FloatLiteral(_) -> PrimitiveType(Float)
  | StringLiteral(_) -> PrimitiveType(String)
  | Nil -> NilType
  | Neg(e) -> (let t = type_from_expr symbols e in match t with 
                        | PrimitiveType(Int) -> PrimitiveType(Int)
                        | PrimitiveType(Float) -> PrimitiveType(Float)
                        | _ -> raise(Failure("Cannot take negative of type other
                        than int or float")))
  | Unop(e, _) -> type_from_expr symbols e
  | ArrayAccess(e1, e2) -> (ignore(let t2 = type_from_expr symbols e2 in match t2 with
                                  | PrimitiveType(Int) -> ()
                                  | _ -> raise(Failure("Index for array must be
                                  int primitive")));

                                  let t1 = type_from_array_access symbols expr in 
                                  let depth = (get_array_access_depth 1 e1) in
                                  match (t1) with 
                                  | PointerType(base, num) ->
                                          if (num < depth) then
                                          ( raise(Failure("Too deep array
                                          access"))) else ( if (depth = num)
                                          then (base) else (PointerType(base,
                                          num - depth)))

                                               
                                  | _ -> raise(Failure("Attempting array access
                                  for non pointer type")))       
  | Binop(e1, _, e2) -> let t1 = type_from_expr symbols e1 in 
                        let t2 = type_from_expr symbols e2 in 
                        ignore (check_compatible_types symbols t1 t2);
                        type_from_expr symbols e1
  | Make(typ_, _) -> (match typ_ with 
                        | ArrayType(type_of_array, pointer, expr) -> (
                               ignore(let type_of_expr = type_from_expr symbols expr in (
                                match type_of_expr with 
                                | PrimitiveType(Int) -> ()
                                | _ -> raise(Failure("Attempting to  allocate memory of
                                non integral size"))
                                ));
                               
                                type_of_array_type symbols typ_
                         )
                        | PointerType(base_type, count) ->
                                        PointerType(base_type, count + 1)
                        | _ -> PointerType(typ_, 1))
  | Clean(e) -> ignore(let t1 = type_from_expr symbols e in match t1 with 
                        | PointerType(base_type, count) -> ()
                        | _ -> raise(Failure("Cannot clean a non pointer"))); PrimitiveType(Void)
  | Call(e, Id(Identifier(id)), _) -> (match e with 
                | Noexpr -> (

                                if (StringMap.mem id symbols) then 
                                        let sym = StringMap.find id symbols in 
                                        match sym with
                                            FuncSymbol(_, fdecl) ->
                                                type_from_declaration_specifiers fdecl.return_type 
                                          | AnonFuncSymbol(_, t) -> t
                                 else
                                        raise(Failure("Calling function: " ^ id
                                        ^ "which is undefined"))
                            )
                | _ ->  (
                           let typ_ = type_from_expr symbols e in
                           (match(typ_) with 
                           | CustomType(name) -> (let fdecl =
                                   get_fdecl_for_receiver name symbols id in
                           type_from_declaration_specifiers fdecl.return_type)
                           | PointerType(CustomType(name), 1) -> (let fdecl =
                                   get_fdecl_for_receiver name symbols id in 
                           type_from_declaration_specifiers fdecl.return_type)
                           | _ -> raise(Failure("Invalid type making method
                           call")))))

  | CompareExpr(_, _, _) -> PrimitiveType(Int)
  | Postfix(e1, _) -> type_from_expr symbols e1
  | MemAccess(expr, Identifier(t)) -> let typ_ = type_from_expr symbols expr in
                                                 type_from_mem_access typ_ t symbols 
  | Id(id) -> type_from_identifier symbols id
  | AsnExpr(expr, _, _) -> type_from_expr symbols expr
  | Super(_) -> raise(Failure("Super defined outside of head of constructor"))
  | Deref(e) -> (let typ_ = type_from_expr symbols e in 
                        match typ_ with 
                        | PointerType(base_type, count) ->
                                        if (count = 1) then 
                                                 base_type
                                        else
                                            PointerType(base_type, count-1)
                       | _ -> raise(Failure("Dereferencing non pointer type")))
  | Pointify(e) -> (let typ_ = type_from_expr symbols e in 
                                match typ_ with 
                                | PointerType(base_type, count) ->
                                                PointerType(base_type, count+1)
                                                (* Because this isn't recursive
                                                 * we need to check if the
                                                 * existing type is a pointer
                                                 * and then add count to the
                                                 * existing pointer type *)
                                | CustomType(s) -> if (is_interface symbols
                                (Identifier(s))) then raise(Failure("Cannot make
                                pointer out of interface")) else
                                        PointerType(typ_, 1)
                                | _ -> (match e with
                                              | Id(id) ->  PointerType(typ_, 1)
                                              | _ -> raise(Failure("Cannot make pointer out of non-identifier"))))
  | AnonFuncDef(adef) -> type_from_anon_def adef
  | Noexpr -> PrimitiveType(Void)                                               

let rec type_list_from_expr_list symbols elist = match elist with
      [] -> []
    | [e] -> [type_from_expr symbols e] 
    | h::t -> [type_from_expr symbols h]@(type_list_from_expr_list symbols t)

let receiver_has_func typ_ symbols func =
        let object_symbol = (lookup_symbol_by_id symbols (Identifier(typ_))) in
        let rec has_func symbol func = match symbol with
                | StructSymbol(type_, struct_) -> (match func.receiver with
                               (type2_, id) -> if (type2_ == type_) then () else
                                       if (struct_.extends <> "") then ( let
                                       parent = lookup_symbol_by_id symbols
                                       (Identifier(struct_.extends)) in has_func parent func)
                                       else raise(Failure("method does not have
                                       receiver")))
                | InterfaceSymbol(type_, interface) -> (if (List.mem func
                interface.funcs) then () else raise(Failure("method isn't part
                of interface")))
        in 

        has_func object_symbol func

let check_constructor symbols struct_name params = 
        let struct_symbol = if (StringMap.mem struct_name symbols) then (lookup_symbol_by_id symbols
        (Identifier(struct_name))) else raise(Failure("Calling constructor for
        undeclared struct: " ^ struct_name)) in 

        match struct_symbol with 

        StructSymbol(typ_, struct_) ->
               let constructor = struct_.constructor in 
               let param_list = constructor.constructor_params in
        
               if List.length param_list != List.length params then
                    if (constructor.constructor_name = "") then 
                        raise(Failure("Parameters for constructor not defined"))
               else
                    List.iter2 (check_compatible_types symbols)
                    (type_list_from_func_param_list param_list)
                         (List.map (type_from_expr symbols) params)

  
       | _ -> raise(Failure("not handled\n"))
          
      
               
let rec check_compatible_type_lists symbols tl1 tl2 = match (tl1, tl2) with
    ([], []) -> ()
  | ([x], [y]) -> check_compatible_types symbols x y
  | (h1::t1, h2::t2) -> check_compatible_types symbols  h1 h2;
                        check_compatible_type_lists symbols t1 t2
  | _ -> raise(Failure("check_compatible_type_lists: type lists are incompatible"))

let validate_call_expr expr_list symbols params = 
        let func_param_types = type_list_from_func_param_list params in 
        let exprList = List.map (type_from_expr symbols) expr_list in 
        List.iter2 (check_compatible_types symbols) exprList func_param_types

(* TODO: validate this *)
let rec validate_anon_call_expr expr expr_list symbols program anonSym = match anonSym with
      AnonFuncSymbol(name, _) -> 
          let anonDef = anon_def_from_tsymbol program anonSym in
          check_anon_body anonDef symbols program anonDef.anon_body 
    | _ -> ()

and check_format_string_with_expr_list symbols fmtStr elist =
    let type_from_fmtSpec fmtSpec = match fmtSpec with
          "%s" -> PrimitiveType(String)
        | "%d" -> PrimitiveType(Int)
        | "%f" -> PrimitiveType(Float)
        | "%c" -> PrimitiveType(Char)
        | _ -> raise(Failure("check_format_string_with_expr_list: Error - Invalid format string"))
    in
    let rec get_fmtSpec_list_from_string str =
        try 
            let firstOccurrence = String.index str '%' in 
            let substr = String.sub str firstOccurrence 2 in
            let remainderStr = String.sub str (firstOccurrence + 1) ((String.length str) - (firstOccurrence + 1))  in
            [substr]@(get_fmtSpec_list_from_string remainderStr)
        with Not_found ->
            []
    in
    let rec type_list_from_fmtSpec_list specList = match specList with 
          [] -> []
        | [s] -> [type_from_fmtSpec s]
        | h::t -> [type_from_fmtSpec h]@(type_list_from_fmtSpec_list t)
    in
    let fmtSpecList = get_fmtSpec_list_from_string fmtStr in
    let argTypeList = type_list_from_expr_list symbols elist in
    let fmtTypeList = type_list_from_fmtSpec_list fmtSpecList in
    try
        List.iter2 (fun t1 t2 -> (check_compatible_types symbols t1 t2)) argTypeList fmtTypeList
    with
        Invalid_argument(_) -> raise(Failure("check_format_string_with_expr_list: Error - Number of format specifiers in format string does not match number of arguments"))
      | _ -> raise(Failure("check_format_string_with_expr_list: Unspecified error"))

and check_call_to_printf symbols exprList = match exprList with
      [e] -> if ((type_from_expr symbols e) <> PrimitiveType(String)) then 
                 raise(Failure("check_call_to_printf: Error - If only 1 argument, must be string!"))
             else
                 ()
    | h::t ->
            if ((type_from_expr symbols h) <> PrimitiveType(String)) then 
                 raise(Failure("check_call_to_printf: Error - If only 1 argument, must be string!"))
             else (match h with 
                 StringLiteral(s) ->
                     check_format_string_with_expr_list symbols s t)

and is_literal expr = match expr with 
| Literal(_) -> true
| StringLiteral(_) -> true
| FloatLiteral(_) -> true
| _ -> false

and check_expr symbols program e = match e with
     Id(Identifier(name)) -> if (StringMap.mem name symbols) == false then
                                raise(Failure("Undeclared identifier"))
                             else ()

  | Binop(e1, _, e2) -> let t1 = type_from_expr symbols e1 in
                         let t2 = type_from_expr symbols e2 in 
                         check_compatible_types symbols t1 t2
   | Pointify(expr) -> ()
   | Neg(expr) -> ignore(type_from_expr symbols expr);

   | Literal(_) -> ()
   | StringLiteral(_) -> ()
   | FloatLiteral(_) -> ()
   | ArrayAccess(e1, e2) -> ignore(type_from_expr symbols e1); if (is_literal
   e1) then raise(Failure("Literal expression is not an array")) else ()
   | Deref(expr) -> (let t1 = type_from_expr symbols expr in
                        match t1 with
                        | PointerType(base_type, _) -> ()
                        | _ -> raise(Failure("Dereferencing non pointer")))
   | Super(_) -> raise(Failure("Super is not at the head of a constructor"))
   | Clean(expr) -> (let t1 = type_from_expr symbols expr in
                        match t1 with 
                        | PointerType(base_type, num) -> ()
                        | _ -> raise(Failure("Cleaning a non pointer type"))) 
   | Call(expr, Id(Identifier(id)), expr_list) -> (match expr with 
                | Noexpr ->
                        (if (StringMap.mem id symbols) then 
                            let s =  StringMap.find id symbols in
                            match s with 
                                 FuncSymbol(_, fdecl) ->
                                     if (id = "printf") then
                                         check_call_to_printf symbols expr_list
                                     else
                                       validate_call_expr
                                       expr_list symbols
                                       fdecl.params
                               | AnonFuncSymbol(name, t) ->
                                        ()
                                       (*validate_anon_call_expr expr expr_list symbols program s*)
                         else
                            raise(Failure("Calling function: " ^ id
                            ^ "which is undefined"))
                        )
                | _ ->  (
                           let typ_ = type_from_expr symbols expr in
                           (match(typ_) with 
                           | CustomType(name) -> (let fdecl =
                                   get_fdecl_for_receiver name symbols id in
                           validate_call_expr expr_list symbols
                           fdecl.params)
                           | PointerType(CustomType(name), 1) -> (let fdecl =
                                   get_fdecl_for_receiver name symbols id in 
                                           validate_call_expr expr_list symbols
                                           fdecl.params)
                           | _ -> raise(Failure("Invalid type making method
                           call")))))
   | Unop(e, unop) -> check_expr symbols program e;
                      let te = type_from_expr symbols e in 
                      (match (te, unop) with
                          (PrimitiveType(Void), _) -> raise(Failure("Cannot apply unary operator to void type"))
                        | (PrimitiveType(_), _) -> ()
                        | _ -> raise(Failure("Type/Unary Operator mismatch")))
   | CompareExpr(e1, op, e2) -> (let t1 = type_from_expr symbols e1 in
                                    let t2 = type_from_expr symbols e2 in
                                    match (t1, t2) with 
                                    | (CustomType(a), _) ->
                                                    raise(Failure("Cannot
                                                    compare custom types"))
                                    | (_, CustomType(s)) -> raise(Failure("Cannot
                                    compare custom types"))
                                    | _ -> check_compatible_types symbols 
                                    (type_from_expr symbols e1) (type_from_expr
                                    symbols e2))
   | Postfix(e1, op) -> check_expr symbols program e1;
                      let te = type_from_expr symbols e1 in 
                      (match (te, op) with
                          (PrimitiveType(Void), _) -> raise(Failure("Cannot
                          apply postfix operator to void type"))
                        | (PrimitiveType(_), _) -> ()
                        | _ -> raise(Failure("Type/Postfix Operator mismatch")))
      
   | MemAccess(s, Identifier(t)) -> ignore(let typ_ = type_from_expr symbols s in 
                                               type_from_mem_access typ_ t
                                               symbols);
   | Make(typ_, expr_list) -> (match (typ_, expr_list)  with
                         | (PrimitiveType(s), [a]) -> ()
                         | (ArrayType(array_time, ptr, expr), []) ->
                                         ignore(type_from_expr symbols
                                         (Make(typ_, expr_list))); 
                         | (CustomType(s), e) -> (check_constructor symbols s e)
                         | _ -> raise(Failure("Invalid make")))
   | AsnExpr(expr, asnOp, e) -> ignore(
                                   if (is_literal expr) then
                                           raise(Failure("Cannot assign to
                                           literal"))
                                   else () 
                                );
                              let t1 = type_from_expr symbols e in
                              let t2 = type_from_expr symbols expr in
                              (match (t1, t2) with
                                 (PrimitiveType(Void), _) | (_, PrimitiveType(Void)) -> raise(Failure("Cannot assign to type void"))
                               | (PrimitiveType(_), CustomType(_)) -> raise(Failure("Cannot assign a struct to a primitive type"))
                               | (CustomType(_), PrimitiveType(_)) -> raise(Failure("Cannot assign a primitive type to a struct"))
                               | _ -> check_compatible_types symbols t2 t1)
   | Noexpr -> ()
   | _ -> raise(Failure("unmatched expression"))

and symbols_from_decls decls = List.map symbol_from_declaration decls

and symbols_from_func_params func_params = List.map symbol_from_func_param func_params

and symbol_from_receiver receiver = match receiver with
     | (type_, id) -> VarSymbol(id, PointerType(CustomType(type_), 1))

and type_from_receiver receiver = match receiver with 
     | (typ_, _) -> typ_

and id_from_receiver receiver = match receiver with 
     | (_, id) -> id

and compare_func_params symbols p1 p2 = 
        let p1_types = List.map type_from_func_param p1 in
        let p2_types = List.map type_from_func_param p2 in
        if (List.length p1_types <> List.length p2_types) then
                raise(Failure("Func Param length mismatch")) else
        List.iter2 (check_compatible_types symbols) p1_types p2_types

and compare_func_names n1 n2 = 
        let n1_name = var_name_from_direct_declarator n1 in
        let n2_name = var_name_from_direct_declarator n2 in
        if (n1_name = n2_name) then () else raise(Failure("Function
        Names do not match"))

and compare_func_return_types symbols r1 r2 = 
       check_compatible_types symbols (type_from_declaration_specifiers r1)
  (type_from_declaration_specifiers r2)

and compare_functions symbols f1 f2 =
        let _ = compare_func_names f1.func_name f2.func_name in
        let _ = compare_func_params symbols f1.params f2.params in
        compare_func_return_types symbols f1.return_type f2.return_type

and symbols_from_fdecls fdecls = List.map symbol_from_fdecl fdecls

and get_id_from_symbol = function
     VarSymbol(id, _) -> id
   | FuncSymbol(id, _) -> id
   | StructSymbol(id, _) -> id
   | InterfaceSymbol(id, _) -> id
   | AnonFuncSymbol(id, t) -> id

and symtable_from_symlist symlist =
      List.fold_left (fun m symbol -> 
          if (StringMap.mem (get_id_from_symbol symbol) m) then
              raise(Failure("symlist_to_symtable: Error - redefining variable"))
          else 
              StringMap.add (get_id_from_symbol symbol) symbol  m) StringMap.empty symlist

      
(*let symbols_from_anon_def anonDef = *)
    (*let paramSymbols = symbols_from_func_params anonDef.anon_params in*)
    (*let bodySymbols = symbols_from_s anonDef.anon_body in*)
    (*symtable_from_symlist (paramSymbols@bodySymbols)*)

and merge_symtables s1 s2 = 
    StringMap.merge (fun key v1 v2 ->
        (match v2, v2 with
           x, y -> if x != y then 
                    raise(Failure("merge_symtables: Error - duplicate symbol"))
                   else x
        | None, y -> y
        | x, None -> x)) s1 s2

and get_decls_from_compound_stmt stmt = match stmt with 
        CompoundStatement(x, y) -> x
   | _ -> []

and get_stmts_from_compound_stmt stmt = match stmt with 
        CompoundStatement(x, y) -> y
   | _ -> []

and check_local_declaration symbols decl = match decl with
     Declaration(declspec, InitDeclList([InitDeclaratorAsn(declarator, asnOp,
     expr)])) ->

             let sym = symbol_from_declaration decl in 
             (match sym with
                 VarSymbol(_, t1) -> let t2 = type_from_expr symbols expr in
                                          check_compatible_types symbols t1 t2
                | FuncSymbol(_, func) -> let t1 =
type_from_declaration_specifiers func.return_type in let
                        t2 =  type_from_expr symbols expr
                in check_compatible_types symbols t1 t2)
    | Declaration(declspec, InitDeclList([InitDeclarator(decl)])) -> ()               
    | _ -> raise(Failure("check_local_declaration not supported"))


and check_bool_expr symbols expr = check_compatible_types symbols (type_from_expr symbols
expr) (PrimitiveType(Int))

and add_to_symbol_table tbl decls = 
    List.fold_left (fun m symbol -> 
        if StringMap.mem (get_id_from_symbol symbol) m then
            raise(Failure("redefining variable: " ^ get_id_from_symbol symbol))
        else StringMap.add (get_id_from_symbol symbol) symbol m)
    tbl (symbols_from_decls decls)

and add_symbol_to_symbol_table tbl sym =
    if (StringMap.mem (Astutil.string_of_symbol_simple sym) tbl) then 
       raise(Failure("redefining variable: " ^ get_id_from_symbol sym)) 
    else 
       StringMap.add (Astutil.string_of_symbol_simple sym) sym tbl

and add_symbol_list_to_symbol_table tbl symlist = match symlist with
      [] -> tbl 
    | [x] -> add_symbol_to_symbol_table tbl x 
    | h::t -> let htbl = add_symbol_to_symbol_table tbl h in
              add_symbol_list_to_symbol_table htbl t 

and check_statement func symbol_table program stmt = match stmt with
     Expr(e) -> (match e with 
                        | Make(_, _) -> raise(Failure("Cannot have stand
                        alone make.")) 
                        | _ -> check_expr symbol_table program e)
  | Return(e) -> check_expr symbol_table program e; check_compatible_types symbol_table (type_from_expr symbol_table e)
  (type_from_declaration_specifiers func.return_type)
  | If(e, s1, s2) -> check_bool_expr symbol_table e; check_statement
  func symbol_table program s1;  check_statement func symbol_table program s2
  | EmptyElse -> ()
  | For(e1, e2, e3, st) -> ((match (e1, e3) with 
                                | (Make(_, _), _) -> raise(Failure("Cannot have
                                stand alone make"))
                                | (_, Make(_, _)) -> raise(Failure("Cannot have
                                stand alone make"))
                                | _ -> ());
                                check_expr symbol_table program e2; check_bool_expr symbol_table e2; check_statement
                                func symbol_table program st)
  | While(e, s) -> check_bool_expr symbol_table e; check_statement func symbol_table program  s
  | CompoundStatement(dl, sl) -> let tbl = add_to_symbol_table symbol_table dl
  in List.iter (check_local_declaration tbl) dl; List.iter (check_statement func tbl program ) sl
  | Break -> ()

and check_anon_body anonDef symbol_table program stmt = match stmt with
     Expr(e) -> (match e with 
                        | Make(_, _) -> raise(Failure("Cannot have stand
                        alone make.")) 
                        | _ -> check_expr symbol_table program e)
  | Return(e) -> 
        check_expr symbol_table program e; 
        check_compatible_types symbol_table (type_from_expr symbol_table e) anonDef.anon_return_type
  | If(e, s1, s2) -> 
        check_bool_expr symbol_table e; 
        check_anon_body anonDef symbol_table program s1;
        check_anon_body anonDef symbol_table program s2
  | EmptyElse -> ()
  | For(e1, e2, e3, st) -> ( (match (e1, e3) with 
                                | (Make(_, _), _) -> raise(Failure("Cannot have
                                stand alone make"))
                                | (_, Make(_, _)) -> raise(Failure("Cannot have
                                stand alone make"))
                                | _ -> ());
                                check_expr symbol_table program e2;
                                check_bool_expr symbol_table e2;
                                check_anon_body anonDef symbol_table program st)
  | While(e, s) -> 
        check_bool_expr symbol_table e;
        check_anon_body anonDef symbol_table program  s
  | CompoundStatement(dl, sl) ->
        let tbl = add_to_symbol_table symbol_table dl in 
        List.iter (check_local_declaration tbl) dl; 
        List.iter (check_anon_body anonDef tbl program ) sl
  | Break -> ()

and func_decl_from_anon_func_def anonDef = {
    return_type = DeclSpecTypeSpecAny(anonDef.anon_return_type);
    func_name = DirectDeclarator(Var(Identifier("")));
    receiver = ("", "");
    params = anonDef.anon_params;
    body = anonDef.anon_body
}

and check_anon_func_def symbol_table program anonDef =  
    check_statement (func_decl_from_anon_func_def anonDef) symbol_table program anonDef.anon_body

(* This function checks 1) Is there a cycle in the inheritence tree and 2)
 * checks that all extensions are valid i.e. no extending oneself or a non
 * existenct struct *)

and validate_all_inheritence symbols structs =  
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
                                                destructor =
                                                        parent_struct.destructor;
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

and get_parents symbols struct_ = 
        if (struct_.extends <> "")
        
        then 
                let StructSymbol(_, parent_struct) = StringMap.find
                struct_.extends symbols in 

                [parent_struct] @ (get_parents symbols parent_struct) 
       else 
                []
(* This function gets the constructor of the closest
 * ancestor of struct_.  *)
and get_ancestors_constructor symbols struct_ =
        if (struct_.extends = "")
        
        then struct_.constructor

        else
                let StructSymbol(_, parent_struct) = StringMap.find struct_.extends symbols in
                        if (parent_struct.constructor.constructor_name = "")
                                then get_ancestors_constructor symbols parent_struct
                        else 
                                parent_struct.constructor

and check_void_decl decl = match decl with 
        | Declaration(decl_spec, _) -> 
                        if (type_from_declaration_specifiers decl_spec =
                                PrimitiveType(Void)) then
                                        raise(Failure("Invalid Declaration of
type Void. Trying to declare variable: " ^ var_name_from_declaration decl ^ " as
void"))
                        else
                                ()
        | _ -> raise(Failure("Unhandled Declaration"))

and remove_duplicate_strings string_list = 
        let str_map = List.fold_left (fun acc str_ -> StringMap.add str_ 1 acc)
        StringMap.empty string_list in 

        StringMap.fold (fun str _ acc -> acc @ [str]) str_map []

and get_method_names_for_struct tSymbol_table struct_ =
       if (struct_.extends = "") then List.map (fun fdecl ->
               var_name_from_direct_declarator fdecl.func_name) struct_.methods 
       else (
              let StructSymbol(_, parent_struct) = lookup_symbol_by_id
              tSymbol_table (Identifier(struct_.extends)) in List.rev ( 
              (List.map (fun fdecl -> var_name_from_direct_declarator
              fdecl.func_name) struct_.methods) @ (get_method_names_for_struct
              tSymbol_table
              parent_struct)))

and get_unique_method_names_for_struct tSymbol_table struct_ = 
        remove_duplicate_strings (get_method_names_for_struct tSymbol_table struct_)

and check_child_methods_against_parent symbols child_methods parent_methods =
        let parent_method_map = List.fold_left (fun m parent_decl -> let
        parent_method = var_name_from_direct_declarator parent_decl.func_name in if
                (StringMap.mem parent_method m) then raise(Failure("Redeclaring
       method: " ^ parent_method)) else (StringMap.add parent_method parent_decl m))
        StringMap.empty parent_methods in 

        List.map (fun child_method ->
                let func_name = 
                         var_name_from_direct_declarator child_method.func_name in 
        if (StringMap.mem func_name
        parent_method_map) then (let parent_method = StringMap.find
        func_name parent_method_map in
        try 
        compare_functions symbols child_method
        parent_method
        with 
        _ -> raise(Failure("Declared child method: " ^ func_name ^ " is
        incompatible with parent declaration"))
        ) else ()) child_methods

and update_fields functions symbols structs  = 
            
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
                                destructor = struct_.destructor;
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

                                                let parent_methods =
                                                        List.filter (fun func ->
                                                                if
                                                                        (type_from_receiver
                                                                        func.receiver
                                                                        =
                                                                                parent_struct.struct_name)
                                                                then true else
                                                                        false)
                                                        functions in 
                                               ignore (check_child_methods_against_parent symbols
                                                        strct_methods parent_methods);
   
                                                let StructSymbol(_,
                                                current_struct)  =
                                                        StringMap.find
                                                        strct.struct_name
                                                        sym in

                                                 

                                                let updated_child_struct = {
                                                        struct_name =
                                                                struct_.struct_name;
                                                        members =
                                                                parent_struct.members
                                                                @
                                                                current_struct.members;
                                                        children =
                                                                current_struct.children;
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
                                                        destructor =
                                                                struct_.destructor;
                                                } in 

                                                StringMap.add
                                                struct_.struct_name
                                                (StructSymbol(struct_.struct_name,
                                                updated_child_struct))
                                                sym) symbols parents in
                                                                       
               List.fold_left (fun symbols struct_ -> (update_field functions
               symbols struct_)) symbols structs

(* Updates structs in the program object with the ones populated in symbol table *)
and update_structs_in_program program  =
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

and check_struct_fields struct_ = 
         ignore (List.map check_void_decl struct_.members);

         List.fold_left (fun sym decl -> if
        (StringMap.mem (var_name_from_declaration decl)
        sym) then raise(Failure("Struct field: " ^
        (var_name_from_declaration (decl)) ^ " was redeclared")) else
                StringMap.add (var_name_from_declaration decl) decl sym)
        StringMap.empty struct_.members 

and struct_implements_method struct_ symbol_table interface_method =
        let interface_method_name = var_name_from_direct_declarator
        interface_method.func_name in
        let fdecl = get_fdecl_for_receiver struct_.struct_name
        symbol_table interface_method_name in 
        
        compare_functions symbol_table fdecl interface_method

and check_implements symbol_table struct_ = 
        if (struct_.implements = "") then () else (
                let impl = struct_.implements in 

                if (StringMap.mem impl symbol_table) then
                        let sym = StringMap.find impl symbol_table in 
                        (match sym with 
                        | InterfaceSymbol(_, interface) -> List.iter
                        (struct_implements_method struct_ symbol_table)
                        interface.funcs 
                        | _ -> raise(Failure("Implementing a non-interface"))
                        )
                else raise(Failure("Implementing a non-existent interface"))
       
        )

and convert_constructor_to_fdecl constructor updated_body = 
        {
                return_type = DeclSpecTypeSpecAny(PrimitiveType(Void));
                func_name =
                        DirectDeclarator(Var(Identifier(constructor.constructor_name)));
                body = updated_body;
                params = constructor.constructor_params;
                receiver = ("", "");
        }

and convert_destructor_to_fdecl destructor = 
        {
                return_type = DeclSpecTypeSpecAny(PrimitiveType(Void));
                func_name =
                        DirectDeclarator(Var(Identifier(destructor.destructor_name)));
                body = destructor.destructor_body;
                params = [];
                receiver = ("", "");
        }

and isSuper stmt = match stmt with 
        | Expr(Super(_)) -> true
        | _ -> false

and constructor_has_super constructor = match constructor.constructor_body with 
        | CompoundStatement(d, s) -> (
                match s with 
                | [] -> false
                | [singleton] -> isSuper singleton
                | h::t -> isSuper h)


and constructor_body_filtered_for_super body = match body with 
        | CompoundStatement(d, s) -> (
                match s with
                | [] -> CompoundStatement(d, s)
                | [singleton] -> if (isSuper singleton) then
                        CompoundStatement(d, []) else CompoundStatement(d, s)
                | h::t -> if (isSuper h) then CompoundStatement(d, t) else
                        CompoundStatement(d, s)
        )
        | _ -> raise(Failure("No other constructor body"))

and getSuperExpr stmt = match stmt with 
        | Expr(Super(e_list)) -> Expr(Super(e_list))
        | _ -> Expr(Noexpr)

and get_super_expr body = match body with 
        | CompoundStatement(_, s) -> (
                match s with 
                | [] -> Expr(Noexpr)
                | [singleton] -> getSuperExpr singleton
                | h::t -> getSuperExpr h)

and validate_super super_ symbol_table struct_ =
        let ancestor_constructor = get_ancestors_constructor symbol_table
        struct_ in

        if (ancestor_constructor.constructor_name =
                struct_.constructor.constructor_name) then
                        raise(Failure("Calling super when no parent constructor
                        is defined"))
        else
                match super_ with 
                | Expr(Super(_)) -> let Expr(Super(elist)) = getSuperExpr super_ in 
                validate_call_expr elist symbol_table
                     ancestor_constructor.constructor_params


and check_for_super_in_constructor symbols struct_ = match
struct_.constructor.constructor_body with 
        | CompoundStatement(decls, stmts) -> (
                match (stmts) with 
                | [] -> ()
                | [h] -> if (isSuper h) then validate_super h symbols
                struct_ else ()
                | h::t -> if (isSuper h) then validate_super h symbols
                struct_ else () 
        )

and build_symbol_table program =
       let fdecls = program.functions @ [{
                       return_type = DeclSpecTypeSpec(Int);
                       func_name = DirectDeclarator(Var(Identifier("printf")));
                       params = [FuncParamsDeclared(DeclSpecTypeSpec(String),
                       DirectDeclarator(Var(Identifier("x"))))];
                       receiver = ("", "");
                       body = CompoundStatement([], []);
               }] in
        
        List.fold_left (fun m symbol -> if StringMap.mem
                (get_id_from_symbol symbol) m then
                                raise(Failure("redefining variable: " ^
                        get_id_from_symbol symbol)) else StringMap.add
                        (get_id_from_symbol symbol) symbol m) StringMap.empty
                          (symbols_from_decls program.globals
                         @ symbols_from_fdecls fdecls
                         @ (symbols_from_structs program.structs)
                         @ (symbols_from_interfaces program.interfaces))

and check_constructor_definition_in_struct program struct_ = 
        ignore (List.iter (fun decl -> if (is_assignment_declaration decl) then
                raise(Failure("Cannot have assignment declaration in struct"))
        else ()) struct_.members);

        let symbol_table = build_symbol_table program in 
        let constructor = struct_.constructor in
        let symbols =  List.fold_left (fun symbol_table sym -> if
        (StringMap.mem (get_id_from_symbol sym)
        symbol_table) then raise(Failure("Struct field: " ^
        (get_id_from_symbol sym) ^ " was redeclared")) else
                StringMap.add (get_id_from_symbol sym) sym symbol_table) 
        symbol_table (symbols_from_decls struct_.members @
        symbols_from_func_params constructor.constructor_params) in

        if (constructor.constructor_name = "") then () 
               
        else
                ignore(check_for_super_in_constructor symbols struct_);
                let updated_body = constructor_body_filtered_for_super
                constructor.constructor_body in

                let func = convert_constructor_to_fdecl constructor
                updated_body
                in  

                check_statement func
        symbols program updated_body

and check_destructor_definition program struct_ =
        let symbol_table = build_symbol_table program in 
        let destructor = struct_.destructor in 
        let symbols = List.fold_left (fun symbol_table sym -> if (StringMap.mem
        (get_id_from_symbol sym) symbol_table) then raise(Failure("Struct field: " ^
        (get_id_from_symbol sym) ^ "was redeclared")) else StringMap.add
        (get_id_from_symbol sym) sym symbol_table) symbol_table
        (symbols_from_decls struct_.members) in 

        if (destructor.destructor_name = "") then ()

        else
                let func = convert_destructor_to_fdecl destructor in 

                check_statement func symbols program destructor.destructor_body

and get_method_names struct_ = List.map (fun func -> var_name_from_direct_declarator func.func_name) struct_.methods


and apply_name_to_anon_def (prefix, count) adef = {
    anon_name =  prefix ^ "_" ^ (string_of_int count);
    anon_return_type = adef.anon_return_type;
    anon_params = adef.anon_params;
    anon_body = adef.anon_body;
}

and anon_defs_from_expr (prefix, count) expr = match expr with
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
           (defs1@defs2, count2)
   | Make(_, elist) -> anon_defs_from_expr_list (prefix, count) elist

   | _ -> ([], count) (* Other expression types cannot possibly contain anonymous function definitions *) 

and anon_defs_from_expr_list (prefix, count) elist = match elist with  
     [] -> ([], count)
   | [e] -> anon_defs_from_expr (prefix, count) e
   | h::t ->
           let (defs1, count1) = (anon_defs_from_expr (prefix, count) h) in
           let (defs2, count2) = (anon_defs_from_expr_list (prefix, count1) t) in
           (defs1@defs2, (count2))


and  anon_defs_from_declaration (prefix, count) decl = match decl with
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

and  anon_defs_from_statement (prefix, count) stmt = match stmt with
     Expr(e) -> anon_defs_from_expr (prefix, count) e
   | Return(e) -> anon_defs_from_expr (prefix, count) e
   | EmptyElse -> ([], 0)
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


and anon_defs_from_func_decl (prefix, count) fdecl = 
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

and anon_defs_from_tprogram tprog =
    let (defs, _) = (anon_defs_from_func_decl_list ("_", 0) (List.rev tprog.functions)) in
    List.rev defs

and anon_def_from_tsymbol tprogram tsym =
    match tsym with 
        AnonFuncSymbol(s, _) ->
            let anonDefs = anon_defs_from_tprogram tprogram in
            let dummyDef = {
                anon_name = "PLACEHOLDER_ANON_DEF";
                anon_return_type = PrimitiveType(Void);
                anon_params = [];
                anon_body = CompoundStatement([], [])
            }
            in
            let (found, anonDef) = (List.fold_left
                (fun (isFound, foundDef) def ->
                    (match isFound with
                          true -> (isFound, foundDef)
                        | false -> 
                            if (def.anon_name = s) then
                                (true, def)
                            else
                                (false, foundDef))) (false, dummyDef) anonDefs)
            in
            if (found = true) then
                anonDef
            else
                let errorStr = "anon_def_from_tsymbol: Error - no anonDef with name " ^ s in
                raise(Failure(errorStr))
                            
and compare_anon_defs_ignore_name a1 a2 =
   let b1 = {
       anon_name = "";
       anon_return_type = a1.anon_return_type;
       anon_params = a1.anon_params;
       anon_body = a1.anon_body
   }
   in
   let b2 = {
       anon_name = "";
       anon_return_type = a2.anon_return_type;
       anon_params = a2.anon_params;
       anon_body = a2.anon_body
   }
   in
   (b1 = b2)

and find_name_for_anon_def tprogram anonDef = 
    let anonDefs = anon_defs_from_tprogram tprogram in 
    let find_match (isFound, targetDef) def = 
        if (isFound = true) then 
            (isFound, targetDef) (* Leave alone *)
        else
            if ((compare_anon_defs_ignore_name targetDef def) = true) then
                (true, def)
            else
                (false, targetDef)
    in
    let (found, def) = List.fold_left find_match (false, anonDef) anonDefs in
    if (found = true) then
        def.anon_name
    else
        raise(Failure("find_name_for_anon_def: Error - could not find a matching anonymous function definition"))

and find_struct_name_for_anon_def tprogram anonDef = 
    let name = find_name_for_anon_def tprogram anonDef in
    "S" ^ name

and anon_defs_from_expr_list_no_recursion tprogram elist = 
   List.fold_left (fun acc e ->
                        (match e with
                            AnonFuncDef(anonDef) ->
                                let anonName = find_name_for_anon_def tprogram anonDef in
                                let namedAnonDef = {
                                    anon_name = anonName;
                                    anon_return_type = anonDef.anon_return_type;
                                    anon_params = anonDef.anon_params;
                                    anon_body = anonDef.anon_body
                                }
                                in
                                acc@[namedAnonDef]
                          | _ -> acc)) [] elist

and expr_list_contains_anon_defs_no_recursion elist = 
    let expr_contains_anon_def_at_this_level truthVal expr  = 
        if (truthVal = true) then
            true
        else
            (match expr with
            AnonFuncDef(_) -> true
          | _ -> false)
    in
            List.fold_left expr_contains_anon_def_at_this_level false elist

and call_contains_anon_def call = 
    let rec expr_contains_anon_def_at_this_level truthVal expr  = 
        if (truthVal = true) then
            true
        else
            match expr with
            AnonFuncDef(_) -> true
          | _ -> false
    in
    match call with
        Call(_, _, elist) ->
            List.fold_left expr_contains_anon_def_at_this_level false elist
      | _ -> raise(Failure("call_contains_anon_def: Error - do not pass anything other than a call expression to this function"))

and symbols_from_outside_scope_for_anon_def tprogram anonDef = 
   let rec expr_contains_anon_def symbols anonDef expr = match expr with 
      |  AnonFuncDef(a) ->
            if (compare_anon_defs_ignore_name a anonDef) then 
                (true, symbols)
            else
                (false, symbols)
      | Binop(e1, _, e2) -> 
              let (found, newSyms) = (expr_contains_anon_def symbols anonDef e1) in
              if found then 
                  (found, newSyms)
              else 
                  let (found, newSyms) = (expr_contains_anon_def newSyms anonDef e2) in
                  if found then 
                      (true, newSyms)
                  else (false, symbols)
      | AsnExpr(e1, _, e2) ->  
              let (found, newSyms) = (expr_contains_anon_def symbols anonDef e1) in
              if found then 
                  (true, newSyms)
              else 
                  let (found, newSyms) = (expr_contains_anon_def newSyms anonDef e2) in
                  if found then 
                      (true, newSyms)
                  else (false, symbols)
      | Literal(x) -> (false, symbols)
      | CompareExpr(e1, _, e2) -> 
              let (found, newSyms) = (expr_contains_anon_def symbols anonDef e1) in
              if found then 
                  (found, newSyms)
              else 
                  let (found, newSyms) = (expr_contains_anon_def newSyms anonDef e2) in
                  if found then 
                      (true, newSyms)
                  else (false, symbols)
      | FloatLiteral(_) -> (false, symbols)
      | StringLiteral(_) -> (false, symbols)
      | Postfix(e, _) -> expr_contains_anon_def symbols anonDef e
      | Call(e1, e2, elist) -> 
              let (found, newSyms) = (expr_contains_anon_def symbols anonDef e1) in
              if found then 
                  (true, newSyms)
              else 
                  let (found, newSyms) = (expr_contains_anon_def newSyms anonDef e2) in
                  if found then 
                      (true, newSyms)
                  else 
                      let (found, newSyms) = (expr_list_contains_anon_def newSyms anonDef elist) in
                      if (found = true) then
                          (true, newSyms)
                      else 
                          (false, symbols)
      | Make(_, elist) -> expr_list_contains_anon_def symbols anonDef elist
      | Pointify(e) -> expr_contains_anon_def symbols anonDef e
      | Deref(e) -> expr_contains_anon_def symbols anonDef e
      | MemAccess(e, _) -> expr_contains_anon_def symbols anonDef e
      | Id(_) -> (false, symbols)
      | DeclExpr(decl) -> declaration_contains_anon_def symbols anonDef decl
      | Noexpr -> (false, symbols)
      | Unop(_, _) -> raise(Failure("expr_contains_anon_def: Error - unop not supported"))
      | _ ->  raise(Failure("expr_contains_anon_def: Error - unexpected expression type"))

   and expr_list_contains_anon_def symbols anonDef elist = match elist with
        [] -> (false, symbols)
      | [e] -> expr_contains_anon_def symbols anonDef e
      | h::t -> let (found, newSyms) = expr_contains_anon_def symbols anonDef h in
                if found then 
                    (true, newSyms) 
                else if
                    let (found, newSyms) = expr_list_contains_anon_def symbols anonDef t in
                    found then (true, newSyms)
                else (false, symbols)

   and init_declarator_contains_anon_def symbols anonDef initDecl = match initDecl with
        InitDeclaratorAsn(_, _, e) -> expr_contains_anon_def symbols anonDef e
      | InitDeclList(idlist) -> init_declarator_list_contains_anon_def symbols anonDef idlist
      | InitDeclarator(_) -> (false, symbols) 
      | _ -> raise(Failure("init_declarator_contains_anon_def: Error - unexpected init_declarator type"))

   and init_declarator_list_contains_anon_def symbols anonDef initDeclList = match initDeclList with
        [] -> (false, symbols)
      | [x] -> init_declarator_contains_anon_def symbols anonDef x
      | h::t -> let (found, newSyms) = init_declarator_contains_anon_def symbols anonDef h in
                if found then
                    (true, newSyms)
                else 
                    let (found, newSyms) = init_declarator_list_contains_anon_def newSyms anonDef t in
                    if found then
                        (true, newSyms)
                    else
                        (false, symbols)

   and declaration_contains_anon_def symbols anonDef decl = match decl with
        Declaration(_, initDecl) -> init_declarator_contains_anon_def symbols anonDef initDecl 

  
   and declaration_list_contains_anon_def symbols anonDef declList = match declList with
        [] -> (false, symbols)
      | [d] -> declaration_contains_anon_def symbols anonDef d
      | h::t ->
              let (found, newSyms) = declaration_contains_anon_def symbols anonDef h in
              if found then
                  (true, newSyms)
              else
                  let (found, newSyms) = declaration_list_contains_anon_def symbols anonDef t in
                  if found then
                      (true, newSyms)
                  else (false, symbols)
              
   and statement_contains_anon_def symbols anonDef stmt = match stmt with
        Expr(e) -> expr_contains_anon_def symbols anonDef e
      | Return(e) -> expr_contains_anon_def symbols anonDef e
      | If(e, s1, s2) -> let (found, newSyms) = expr_contains_anon_def symbols anonDef e in
                        if found then
                            (true, newSyms)
                        else 
                            let (found, newSyms) = statement_contains_anon_def newSyms anonDef stmt in
                            if found then
                                (true, newSyms)
                            else (false, symbols)
      | CompoundStatement(declList, stmtList) -> 
                        let (found, newSyms) = declaration_list_contains_anon_def symbols anonDef declList in
                                                if found then
                                                    (true, newSyms)
                                                else 
                                                    let (found, newSyms) = statement_list_contains_anon_def newSyms anonDef stmtList in
                                                    if found then
                                                        (true, newSyms)
                                                    else (false, symbols)

   and statement_list_contains_anon_def symbols anonDef stmtList = match stmtList with
        [] -> (false, symbols)
      | [s] -> statement_contains_anon_def symbols anonDef s
      | h::t -> 
              let (found, newSyms) = statement_contains_anon_def symbols anonDef h in
              if found then
                  (true, newSyms)
              else
                  let (found, newSyms) = statement_list_contains_anon_def symbols anonDef t in
                  if found then
                      (true, newSyms)
                  else (false, symbols)

   and fdecl_contains_anon_def symbols anonDef fdecl =
       match fdecl.body with
            CompoundStatement(declList, stmtList) -> 
               let psymbols = symbols_from_func_params fdecl.params in
               let bsymbols = symbols_from_decls declList in
               let (found, newSyms) = declaration_list_contains_anon_def (symbols@psymbols@bsymbols) anonDef declList in
               if (found = true) then
                  (true, newSyms)
               else
                   let (found, newSyms) = statement_list_contains_anon_def (symbols@psymbols@bsymbols) anonDef stmtList in
                   if (found = true) then
                       (true, newSyms)
                   else (false, symbols)

   and fdecl_list_contains_anon_def symbols anonDef fdeclList = match fdeclList with
        [] -> (false, symbols)
      | [f] -> fdecl_contains_anon_def symbols anonDef f
      | h::t ->
              let (found, newSyms) = fdecl_contains_anon_def symbols anonDef h in
              if found then
                  (true, newSyms)
              else
                  let (found, newSyms) = fdecl_list_contains_anon_def symbols anonDef t in
                  if found then
                      (true, newSyms)
                  else (false, symbols)
   
   in
   let program_contains_anon_def anonDef program =
        let globals = symbols_from_decls program.globals in 
        fdecl_list_contains_anon_def globals anonDef program.functions

   in

   let (found, symlist) = program_contains_anon_def anonDef tprogram in
   if (found = false) then 
       raise(Failure("Error: program does not contain anonDef"))
   else
       symlist

and print_anon_def anonDef = 
    Printf.printf "\n%s\n" (Astutil.string_of_anon_def anonDef)

and print_anon_defs = function
      [] -> ()
    | [x] -> print_anon_def x
    | h::t -> print_anon_def h; print_anon_defs t

let check_structs_satisfy_interfaces program = 
        let symbol_table = build_symbol_table program in 

        List.map (check_implements symbol_table) program.structs 

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
                       in ignore(List.map check_struct_fields
                       program.structs);

       ignore (List.map (check_constructor_definition_in_struct program)
        program.structs);

       ignore (List.map (check_destructor_definition program)
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
     
       check_structs_satisfy_interfaces program;
       
       let fdecls = program.functions @ [{
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
              
                ignore(if (func.receiver <> ("", "")) then (if StringMap.mem (fst
                func.receiver) symbol_table then () else
                        raise(Failure("receiver: " ^ (fst func.receiver) ^ " is
                        not defined"))));

                List.iter (check_local_declaration symbol_table)
                (get_decls_from_compound_stmt func.body);

                List.iter (check_statement func symbol_table program )
                (get_stmts_from_compound_stmt func.body); 
                        
        in List.iter check_function program.functions;
