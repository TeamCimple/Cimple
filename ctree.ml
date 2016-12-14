open Ast

module StringMap = Map.Make(String)

type cIdentifier = CIdentifier of string

type cPrimitive =
    Cvoid 
  | Cchar
  | Cshort 
  | Cint 
  | Clong 
  | Cfloat 
  | Cdouble

type cProgram = {
        cstructs: cStruct list;
        cglobals: cDeclaration list;
        cfunctions: cFunc list;
}

and cStruct = {
  struct_name: string;
  struct_members: cSymbol list;
  method_to_functions: cFunc StringMap.t;
}

and cFuncSignature = {
    func_return_type: cType;
    func_param_types: cType list; 
}

and cFunc = {
  cfunc_name: string;
  cfunc_body: cStatement;
  cfunc_params: cFuncParam list;
  creturn_type: cType;
}

and cFuncDecl = {
  cfdecl_name: string;
  cfdecl_params: cType list;
  cfdecl_return_type: cType;
}

and cNonPointerType =
    CPrimitiveType of cPrimitive
  | CStruct of string

and cPointer = 
    CPointer of cNonPointerType
  | CPointerPointer of cPointer

and cType = 
    CType of cNonPointerType
  | CPointerType of cType * int
  | CFuncPointer of cFuncSignature 

and cExpr = 
    CBinop of cExpr * tOperator * cExpr
   | CAsnExpr of cExpr * tAssignmentOperator * cExpr
   | CLiteral of int
   | CFloatLiteral of float
   | CStringLiteral of string
   | CCastExpr of cType * cExpr
   | CPostfix of cExpr * tPostfixOperator
   | CCall of int * cExpr * cExpr * cExpr list (* The int field is a flag to
   indiciate it is a pointer dereference *)
   | CAlloc of cType * int
   | CCompareExpr of cExpr * tLogicalOperator * cExpr
   | CPointify of cExpr
   | CMemAccess of int * cExpr * cIdentifier (* The int field is a flag to
   indicate it is a pointer dereference *)
   | CId of cIdentifier
   | CDeclExpr of cDeclaration
   | CNoexpr

and cStatement = 
    CExpr of cExpr
    | CEmptyElse
    | CReturn of cExpr
    | CCompoundStatement of cDeclaration list * cStatement list
    | CIf of cExpr * cStatement * cStatement
    | CFor of cExpr * cExpr * cExpr * cStatement
    | CWhile of cExpr * cStatement
    | CBreak

and cDirectDeclarator =
    CVar of cIdentifier

and cDeclarator = 
    CDirectDeclarator of cDirectDeclarator

and cInitDeclarator = 
    CInitDeclarator of cDeclarator
   |  CInitDeclaratorAsn of cDeclarator * tAssignmentOperator * cExpr 

and cDeclarationSpecifiers = 
    CDeclSpecTypeSpecAny of cType

and cFuncParam = cType * cIdentifier    

and cDeclaration = 
    CDeclaration of cDeclarationSpecifiers * cInitDeclarator

and cSymbol = 
    CVarSymbol of string * cType
    | CFuncSymbol of string * cFunc
    | CStructSymbol of string * cStruct

let cStructName_from_tInterface name = 
        String.concat "" ["_interface"; name]

let interface_field_name_in_struct interface_name struct_name = 
        String.concat "" ["_"; (String.concat "_" [interface_name;struct_name])]

let cStructName_from_tStruct name = 
        String.concat "" ["_struct"; name]

(*let cNullFunction = {*)
    (*cfunc_name = "$NULL_FUNCTION";*)
    (*cfunc_body = CCompoundStatement([], []);*)
    (*cfunc_params = [];*)
    (*creturn_type = CType(CPrimitiveType(Cvoid))*)
(*}*)

(*let cNullStruct = {*)
    (*struct_name = "$NULL_STRUCT";*)
    (*struct_members = [];*)
    (*method_to_functions = StringMap.empty*)
(*}*)

let cType_from_tTypeSpec = function
    Void -> CType(CPrimitiveType(Cvoid))  
  | Char -> CType(CPrimitiveType(Cchar))
  | Short -> CType(CPrimitiveType(Cshort))
  | Int -> CType(CPrimitiveType(Cint))
  | Long -> CType(CPrimitiveType(Clong))
  | Float -> CType(CPrimitiveType(Cfloat))
  | Double -> CType(CPrimitiveType(Cdouble))
  | Signed -> raise(Failure("cType_from_tTypeSpec: Error, Signed unsuported at the moment"))
  | Unsigned -> raise(Failure("cType_from_tTypeSpec: Error, Unsigned unsuported at the moment"))
  | String -> CPointerType(CType(CPrimitiveType(Cchar)), 1) 
  | _ -> raise(Failure("cType_from_tTypeSpec: Error, unsupported tTypeSpec"))

let rec cType_from_tType symbol_table = function
    PrimitiveType(typeSpec) -> cType_from_tTypeSpec typeSpec
  | PointerType(base_type, num) -> CPointerType(cType_from_tType symbol_table base_type,
  num)
  | CustomType(s) -> (let sym = StringMap.find s symbol_table in 
                                        match sym with 
                                        | StructSymbol(name, _) ->
                                                        CType(CStruct(cStructName_from_tStruct
                                        name))
                                        | InterfaceSymbol(name, _) ->
                                                        CPointerType(CType(CStruct(cStructName_from_tInterface
                                        name)), 1))
  | AnonFuncType(t, tlist) -> 
          let anonRetType = (cType_from_tType symbol_table t) in 
          let anonParamTypes = List.map (fun x -> (cType_from_tType symbol_table x)) tlist in
          CFuncPointer({ 
              func_return_type = anonRetType;
              func_param_types = anonParamTypes
          })
  | _ -> raise(Failure("Haven't filled out yet"))


let merge_symtables s1 s2 = 
    StringMap.merge (fun key v1 v2 ->
        (match v2, v2 with
           x, y -> if x != y then 
                    raise(Failure("concat_symtables: Error - duplicate symbol"))
                   else x
        | None, y -> y
        | x, None -> x)) s1 s2
            
let id_exists_in_symtable symbols id = 
  try 
    StringMap.find (Astutil.string_of_identifier id) symbols;
    true
  with _ -> false 

let id_exists_in_symlist symlist id = 
  let check_sym_id_equal sym id = 
    match sym with
      VarSymbol(name, _) -> if (name = (Astutil.string_of_identifier id)) then true else false
    | FuncSymbol(name, _) -> if (name = (Astutil.string_of_identifier id)) then true else false
    | AnonFuncSymbol(name, _) -> if ( name = (Astutil.string_of_identifier id)) then true else false
  in
  let compare_symbol_with_id (id, (hasBeenFound, foundSymbol)) sym = 
    match hasBeenFound with 
      false -> if (check_sym_id_equal sym id) == true then (id, (true, sym))
               else (id, (hasBeenFound, foundSymbol))
    | true -> (id, (hasBeenFound, foundSymbol))
  in
  let (_, (isFound, foundSym)) = (List.fold_left compare_symbol_with_id (id, (false, VarSymbol("ERROR_SYMBOL", PrimitiveType(Void)))) symlist) in isFound


let lookup_symbol_from_symlist_by_id symlist id = 
  let check_sym_id_equal sym id = 
    match sym with
      VarSymbol(name, _) -> name == (Astutil.string_of_identifier id)
    | FuncSymbol(name, _) -> name == (Astutil.string_of_identifier id)
    | AnonFuncSymbol(name, _) -> name == (Astutil.string_of_identifier id) 
  in
  let compare_symbol_with_id (id, (hasBeenFound, foundSymbol)) sym = 
    match hasBeenFound with 
      false -> if (check_sym_id_equal sym id) == true then (id, (true, sym))
               else (id, (hasBeenFound, foundSymbol))
    | true -> (id, (hasBeenFound, foundSymbol))
  in
  match (List.fold_left compare_symbol_with_id (id, (false, VarSymbol("ERROR_SYMBOL", PrimitiveType(Void)))) symlist) with
    (_, (true, foundSym)) -> foundSym
  | _ -> raise(Failure("lookup_symbol_from_symlist_by_id: Error, symbol not in table."))


let cDeclarationSpecifiers_from_tDeclarationSpecifiers symbol_table tDeclSpecs = function
        | DeclSpecTypeSpecAny(tType) ->
                        CDeclSpecTypeSpecAny(cType_from_tType symbol_table tType)

let cDeclaration_from_tFdecl symbol_table fdecl = 
        let first_argument = [CPointerType(CType(CPrimitiveType(Cvoid)), 1)] in
        let cfunc_param_types = first_argument @ List.map (cType_from_tType symbol_table)
                                     (Semant.type_list_from_func_param_list fdecl.params) in 
        let cfunc_return_type =
                cType_from_tType symbol_table (Semant.type_from_declaration_specifiers
                fdecl.return_type) in
        let func_signature = {
                                 func_return_type = cfunc_return_type;
                                 func_param_types = cfunc_param_types;
                                } in
         
        CVarSymbol((Semant.var_name_from_direct_declarator fdecl.func_name), CFuncPointer(func_signature))

(* The C Struct corresponding to the Cimple Interface consists of 
 * 1) Function pointers instead of methods. The first argument is a void star *)

let cStruct_from_tInterface symbol_table interface = 
        let cBodySymbol = [CVarSymbol("body",
        CPointerType(CType(CPrimitiveType(Cvoid)),
        1))] in (* This is the void * body that we apply to all the functions *) 
        let cSymbols = List.map (cDeclaration_from_tFdecl symbol_table) interface.funcs in 
        {
                struct_members = cBodySymbol @ cSymbols;
                struct_name = cStructName_from_tInterface interface.name;
                method_to_functions = StringMap.empty
        }

let cSymbol_from_Implements implements =
        let cstruct_name = cStructName_from_tInterface implements in
        CVarSymbol(cstruct_name, CPointerType(CType(CStruct(cstruct_name)), 1))

let cFuncParam_from_tFuncParam symbol_table tFuncParam = (cType_from_tType
symbol_table (Semant.type_from_func_param tFuncParam),
(CIdentifier(Semant.var_name_from_func_param tFuncParam)))

let create_cfunc_param_for_receiver receiver = 
        (CPointerType(CType(CPrimitiveType(Cvoid)), 1),
        CIdentifier("_body"))

 let create_initial_cast_decl receiver =
        let cstruct_name = cStructName_from_tStruct (fst receiver) in  
        CDeclaration(CDeclSpecTypeSpecAny(CPointerType(CType(CStruct(cstruct_name)),
        1)), CInitDeclaratorAsn(CDirectDeclarator(CVar(CIdentifier(snd receiver)))
        , Asn, CCastExpr(CPointerType(CType(CStruct(cstruct_name)), 1),
        CId(CIdentifier("_body")))))

let cFunc_from_tFunc symbol_table tFunc = 
        {
                creturn_type = cType_from_tType symbol_table
                (Semant.type_from_declaration_specifiers tFunc.return_type);

                cfunc_params = (List.map (cFuncParam_from_tFuncParam
                symbol_table) tFunc.params);

                cfunc_body = CCompoundStatement([], []);

                cfunc_name = Semant.var_name_from_direct_declarator
                tFunc.func_name;
        }

let rec cSymbol_from_sSymbol symbol_table sym = match sym with
    VarSymbol(s, t) -> CVarSymbol(s, (cType_from_tType symbol_table t))
  | FuncSymbol(s, fdecl) -> CFuncSymbol(s, (cFunc_from_tFunc symbol_table fdecl))
  | StructSymbol(s, strct) ->
          let (newStrct, _) = cStruct_from_tStruct symbol_table strct in
          CStructSymbol(s, newStrct)
  | _ -> raise(Failure("Not completed"))


 (*------------------- Function struct_members_from_anon_body-------------------------
 * This function returns a list of Ast.sSymbols representing the variables referenced within the body of
 * an anonymous function that are declared outside of it's scope. This list will form the data
 * members of a special c struct that will be passed to a normal c function whenevever 
 * an anonymous function in cimple is instantiated.
 *
 * Parameters:
         * symbols: A StringMap of symbols from outside the scope of the anonymous function def
         * psymbols: A symbol table of parameters to this anonymous function
         * members: A list of function parameters declared in the anon function definition
         * body: An Ast.tStatement (specifically a CompoundStatement) that is the body of the anonymous function
 *-------------------------------------------------------------------------------- *)

and struct_members_from_anon_body symbols psymbols members body = 

  let rec print_member m = Printf.printf "%s\n" (Astutil.string_of_symbol m)

  and print_member_list mlist = match mlist with
        [] -> ()
      | [x] -> print_member x
      | h::t -> print_member h; print_member_list t
  in
 
  let symbol_is_capturable = function
        VarSymbol(_, _) -> true
      | _ -> false
  in

  let rec members_from_expr symbols psymbols members e = match e with 
      Id(id) -> if (id_exists_in_symtable symbols id) = true then
                  let sym = Semant.lookup_symbol_by_id symbols id in
                  if (symbol_is_capturable sym) then [sym] else []
                else if (id_exists_in_symtable psymbols id) then []
                else if (id_exists_in_symlist members id) = true then
                  []
                else (match id with 
                  Identifier(s) -> 
                      print_member_list members; 
                      raise(Failure("members_from_expr: Error - undeclared symbol '" ^ s ^ "'")))
   |  Binop(e1, _, e2) -> let e1Members = members_from_expr symbols psymbols members e1 in
                           let e2Members = members_from_expr symbols psymbols (members@e1Members) e2 in
                           e1Members@e2Members
   |  AsnExpr(e1, _, e2) -> let e1Members = members_from_expr symbols psymbols members e1 in
                            let e2Members = members_from_expr symbols psymbols (members@e1Members) e2 in
                            e1Members@e2Members
   |  Postfix(e1, _) -> let e1Members = members_from_expr symbols psymbols members e1 in
                            e1Members
   |  Call(_, e, elist) -> let eMembers = members_from_expr symbols psymbols members e in 
                           let elistMembers = members_from_expr_list symbols psymbols (members@eMembers) elist in
                           eMembers@elistMembers
   | Make(_, elist) -> members_from_expr_list symbols psymbols members elist
   | Pointify(e) -> members_from_expr symbols psymbols members e
   | MemAccess(e, id2) -> let id1Members = members_from_expr symbols psymbols members e in 
                            let id2Members = members_from_expr symbols psymbols 
                            (members@id1Members) e in
                            id1Members@id2Members
   | AnonFuncDef(def) -> raise(Failure("members_from_expr: Error - nested anonymous functions not supported yet"))
   | DeclExpr(decl) -> members_from_declaration symbols psymbols members decl
   | _  -> []

   and members_from_expr_list symbols psymbols members elist = match elist with
     [] -> []
   | [x] -> members_from_expr symbols psymbols members x
   | h::t -> let hMembers = members_from_expr symbols psymbols members h in
             let tMembers = members_from_expr_list symbols psymbols members t in
             hMembers@tMembers
   and members_from_init_declarator symbols psymbols members initDecl =
    match initDecl with 
       InitDeclaratorAsn(_, _, e) -> members_from_expr symbols psymbols members e
     | InitDeclList(l) -> members_from_init_declarator_list symbols psymbols members l
      
   and members_from_init_declarator_list symbols psymbols members declList =
    match declList with 
      [] -> members_from_expr symbols psymbols members Noexpr
    | [x] -> members_from_init_declarator symbols psymbols members x
    | h::t -> let hmembers = members_from_init_declarator symbols psymbols members h in
              hmembers@(members_from_init_declarator_list symbols psymbols (members@hmembers) t)
    
   and members_from_declaration symbols psymbols members decl = match decl with
     Declaration(_, initDecl) -> 
         (*let updatedSymbols = Semant.add_to_symbol_table symbols [decl] in*)
         members_from_init_declarator symbols psymbols members initDecl 
   | _ -> [] (* Other types of declarations wouldn't reference variables from outside scope *)

   and members_from_declaration_list symbols psymbols members declList = match declList with
      [] -> []
   | [x] -> members@(members_from_declaration symbols psymbols members x)
   | h::t -> let hmembers = members_from_declaration symbols psymbols members h in
             hmembers@(members_from_declaration_list symbols psymbols (members@hmembers) t)
    
   and members_from_statement_list symbols psymbols members stmtList = match stmtList with
     [] -> members
   | [x] -> members@(members_from_statement symbols psymbols members x)
   | h::t -> let hmembers = members_from_statement symbols psymbols members h in
             (hmembers)@(members_from_statement_list symbols psymbols (members@hmembers) t)

   and members_from_statement symbols psymbols members stmt = match stmt with 
     CompoundStatement(decls, stmtList) -> 
       let dmembers = members@(members_from_declaration_list symbols psymbols members decls) in
       members_from_statement_list symbols psymbols dmembers stmtList
   | Expr(e) -> members_from_expr symbols psymbols members e
   | Return(e) -> members_from_expr symbols psymbols members e
   | If(e, s1, s2) -> let eMembers = members_from_expr symbols psymbols members e in 
                      let s1Members = members_from_statement symbols psymbols (members@eMembers) s1 in 
                      let s2Members = members_from_statement symbols psymbols (members@eMembers@s1Members) s2 in
                      eMembers@s1Members@s2Members
   | For(e1, e2, e3, s) -> let e1Members = members_from_expr symbols psymbols members e1 in
                           let e2Members = members_from_expr symbols psymbols (members@e1Members) e2 in
                           let e3Members = members_from_expr symbols psymbols (members@e1Members@e2Members) e3 in
                           let sMembers = members_from_statement symbols psymbols (members@e1Members@e2Members@e3Members) s in
                           e1Members@e2Members@e3Members
   | While(e, s) -> let eMembers = members_from_expr symbols psymbols members e in
                    let sMembers = members_from_statement symbols psymbols (members@eMembers) s in
                    eMembers@sMembers 
   | _ -> []
  in

  members_from_statement symbols psymbols members body

(* --------------------------Function capture_struct_from_anon_def ------------------------
 * Returns a C struct to be used as a copy of the variables used within the body of an 
 * anonymous function that were declared outside of its scope.
 *
 * Parameters:
         * symbols: A hash table of the symbols declared in the outside scope.
         * structName: a string that will become the name of the struct in the resulting 
         *              C Program.
         * def: The Ast.tAnonFuncDef whose body we are looking through to find captured variables
 * ------------------------------------------------------------------------*)

and capture_struct_from_anon_def symbols def = 
  let rec symconvert m = cSymbol_from_sSymbol symbols m in
  let param_symbols = Semant.symbols_from_func_params def.anon_params in
  let param_symtable = (Semant.symtable_from_symlist param_symbols) in
  let body_symbols = (Semant.symbols_from_decls (Semant.get_decls_from_compound_stmt def.anon_body)) in
  let updated_param_symtable = (Semant.add_symbol_list_to_symtable body_symbols param_symtable) in
  let updated_symtable = (Semant.add_symbol_list_to_symtable (param_symbols@body_symbols) symbols) in
  Printf.printf "Printing body symbol table for: %s\n" (def.anon_name);
  Astutil.print_symbol_table updated_symtable;
  Printf.printf "-----------End printing of symbol table\n";
    {
      struct_name = def.anon_name;
      struct_members = (List.map symconvert (struct_members_from_anon_body symbols updated_param_symtable [] def.anon_body));
      method_to_functions = StringMap.empty;
    }

and capture_struct_list_from_anon_def_list symbols defList = match defList with
      [] -> []
    | [x] -> [capture_struct_from_anon_def symbols x]
    | h::t -> [capture_struct_from_anon_def symbols h]@capture_struct_list_from_anon_def_list symbols t

and cFunc_from_tMethod cStruct_Name tFuncName = String.concat "_" [cStruct_Name;tFuncName]

and cStruct_from_tStruct symbol_table tStruct = 
        let symconvert m = cSymbol_from_sSymbol symbol_table m in
        let defaultStructMemberSymbols = List.map symconvert (List.map (Semant.symbol_from_declaration) tStruct.members) in
       
        (* If there is an interface then add a struct member corresponding to
         * the interface to our struct *)
        let cStructMemberSymbols = if (Semant.get_interface_for_struct
        tStruct.struct_name symbol_table <> "") then
                [cSymbol_from_Implements tStruct.implements] @
                defaultStructMemberSymbols else defaultStructMemberSymbols in

               let (methods_to_cfunctions, cfuncs) = (List.fold_left (fun (sym, cfunc_list) method_ -> 
                                            (let tfunc_name =
                                                    Semant.var_name_from_direct_declarator
                                               method_.func_name in
                                            
                                           let initial_void_param = 
                                                        create_cfunc_param_for_receiver
                                                        method_.receiver in

                                           let init_cast_decl =
                                                   create_initial_cast_decl
                                                   method_.receiver in

 
                                            let cfunc = {
                                                creturn_type = (cType_from_tType
                                                symbol_table
                                                (Semant.type_from_declaration_specifiers
                                                method_.return_type));
                                                
                                                cfunc_params =
                                                        [initial_void_param] @ (List.map
                                                (cFuncParam_from_tFuncParam
                                                symbol_table) method_.params);

                                                cfunc_body =
                                                        CCompoundStatement([init_cast_decl],
                                                []);
        
                                                cfunc_name = cFunc_from_tMethod
                                                (cStructName_from_tStruct
                                                tStruct.struct_name) tfunc_name;

                                         } in (StringMap.add tfunc_name cfunc
                                         sym, cfunc_list @ [cfunc])))
                                            (StringMap.empty, []) tStruct.methods) in

        ({
                struct_name = cStructName_from_tStruct tStruct.struct_name;
                struct_members = cStructMemberSymbols;
                method_to_functions = methods_to_cfunctions;
        }, cfuncs)

let cDeclarationSpecifiers_from_tDeclarationSpecifiers symbol_table tDeclSpecs = function
        | DeclSpecTypeSpecAny(tType) ->
                        CDeclSpecTypeSpecAny(cType_from_tType symbol_table tType)

let cDeclaration_from_tFdecl symbol_table fdecl = 
        let first_argument = [CPointerType(CType(CPrimitiveType(Cvoid)), 1)] in
        let cfunc_param_types = first_argument @ List.map (cType_from_tType symbol_table)
                                     (Semant.type_list_from_func_param_list fdecl.params) in 
        let cfunc_return_type =
                cType_from_tType symbol_table (Semant.type_from_declaration_specifiers
                fdecl.return_type) in
        let func_signature = {
                                 func_return_type = cfunc_return_type;
                                 func_param_types = cfunc_param_types;
                                } in
         
        CVarSymbol((Semant.var_name_from_direct_declarator fdecl.func_name), CFuncPointer(func_signature))

(* The C Struct corresponding to the Cimple Interface consists of 
 * 1) Function pointers instead of methods. The first argument is a void star *)

let cStruct_from_tInterface symbol_table interface = 
        let cBodySymbol = [CVarSymbol("body",
        CPointerType(CType(CPrimitiveType(Cvoid)),
        1))] in (* This is the void * body that we apply to all the functions *) 
        let bols = List.map (cDeclaration_from_tFdecl symbol_table) interface.funcs in 
        {
                struct_members = cBodySymbol @ bols;
                struct_name = cStructName_from_tInterface interface.name;
                method_to_functions = StringMap.empty
        }

let bol_from_Implements implements struct_name =
        let cstruct_name = cStructName_from_tInterface implements in
        let interface_field_name = interface_field_name_in_struct implements
        struct_name in
        CVarSymbol(interface_field_name, CType(CStruct(cstruct_name)))

let cFuncParam_from_tFuncParam symbol_table tFuncParam = (cType_from_tType
symbol_table (Semant.type_from_func_param tFuncParam),
(CIdentifier(Semant.var_name_from_func_param tFuncParam)))

let create_cfunc_param_for_receiver receiver = 
        (CPointerType(CType(CPrimitiveType(Cvoid)), 1),
        CIdentifier("_body"))

 let create_initial_cast_decl receiver =
        let cstruct_name = cStructName_from_tStruct (fst receiver) in  
        CDeclaration(CDeclSpecTypeSpecAny(CPointerType(CType(CStruct(cstruct_name)),
        1)), CInitDeclaratorAsn(CDirectDeclarator(CVar(CIdentifier(snd receiver)))
        , Asn, CCastExpr(CPointerType(CType(CStruct(cstruct_name)), 1),
        CId(CIdentifier("_body")))))

let cFunc_from_tFunc symbol_table tFunc = 
        {
                creturn_type = cType_from_tType symbol_table
                (Semant.type_from_declaration_specifiers tFunc.return_type);

                cfunc_params = (List.map (cFuncParam_from_tFuncParam
                symbol_table) tFunc.params);

                cfunc_body = CCompoundStatement([], []);

                cfunc_name = Semant.var_name_from_direct_declarator tFunc.func_name;
        }

let cFunc_from_tMethod tStructName tFuncName =
        Semant.symbol_table_key_for_method tStructName tFuncName

let cStruct_from_tStruct symbol_table tStruct = 
        let defaultStructMemberSymbols = List.map (cSymbol_from_sSymbol
        symbol_table) (List.map (Semant.symbol_from_declaration)
        tStruct.members) in 

        (* If there is an interface then add a struct member corresponding to
         * the interface to our struct *)
        let cStructMemberSymbols = if (Semant.get_interface_for_struct
        tStruct.struct_name symbol_table <> "") then
                [bol_from_Implements (Semant.get_interface_for_struct
                tStruct.struct_name symbol_table) tStruct.struct_name] @
                defaultStructMemberSymbols else defaultStructMemberSymbols in

               let (methods_to_cfunctions, cfuncs) = (List.fold_left (fun (sym, cfunc_list) method_ -> 
                                            (let tfunc_name =
                                                    Semant.var_name_from_direct_declarator
                                               method_.func_name in
                                            
                                           let initial_void_param = 
                                                        create_cfunc_param_for_receiver
                                                        method_.receiver in

                                           let init_cast_decl =
                                                   create_initial_cast_decl
                                                   method_.receiver in

 
                                            let cfunc = {
                                                creturn_type = (cType_from_tType
                                                symbol_table
                                                (Semant.type_from_declaration_specifiers
                                                method_.return_type));
                                                
                                                cfunc_params =
                                                        [initial_void_param] @ (List.map
                                                (cFuncParam_from_tFuncParam
                                                symbol_table) method_.params);

                                                cfunc_body =
                                                        CCompoundStatement([init_cast_decl],
                                                []);
        
                                                cfunc_name = cFunc_from_tMethod 
                                                tStruct.struct_name tfunc_name;

                                         } in (StringMap.add tfunc_name cfunc
                                         sym, cfunc_list @ [cfunc])))
                                            (StringMap.empty, []) tStruct.methods) in

        ({
                struct_name = cStructName_from_tStruct tStruct.struct_name;
                struct_members = cStructMemberSymbols;
                method_to_functions = methods_to_cfunctions;
        }, cfuncs)

let cFunction_from_tMethod object_type method_ tSymbol_table = 
        match object_type  with 
        | CustomType(name) -> ( let typ_symbol  =
                Semant.lookup_symbol_by_id tSymbol_table (Identifier(name)) in match
                typ_symbol with 
                | StructSymbol(_, _) -> cFunc_from_tMethod
                (name) method_
                | InterfaceSymbol(_, _) -> method_)
        | PointerType(CustomType(name), 1) -> ( let typ_symbol  =
                Semant.lookup_symbol_by_id tSymbol_table (Identifier(name)) in match
                typ_symbol with 
                | StructSymbol(_, _) -> cFunc_from_tMethod
                ( name) method_ 
                | InterfaceSymbol(_, _) -> method_)
        | _ -> raise(Failure("not done"))
(*
 * This function takes a cimple expression and returns the pair (C expression,
 * statement[]). The idea is that some cimple expression may generate multiple
 * assignment statements such as a make expression with a constructor.
 * Expressions should not create more declarations, only declarations create
 * more declarations.
 *)
let rec update_expr texpr tSymbol_table  = match texpr with
        | Binop(e1, op, e2) -> (let (updated_e1, e1_stmts) = update_expr e1
                                tSymbol_table in 
                        let (updated_e2, e2_stmts) = update_expr e2
                        tSymbol_table  in 

                        (CBinop(updated_e1, op, updated_e1), e1_stmts @
                        e2_stmts))
        | CompareExpr(e1, op, e2) -> ( let (updated_e1, e1_stmts) = update_expr
        e1 tSymbol_table in let (updated_e2, e2_stmts) = update_expr e2
        tSymbol_table in 

        (CCompareExpr(updated_e1, op, updated_e2), e1_stmts @ e2_stmts))

        | AsnExpr(e1, op, e2)  -> (let (updated_e1, e1_stmts) = update_expr e1
                                tSymbol_table in 
                        let (updated_e2, e2_stmts) = update_expr e2
                        tSymbol_table  in 

                        let e1_type = Semant.type_from_expr tSymbol_table e1 in 

                        let e2_type = Semant.type_from_expr tSymbol_table e2 in 

                        match (e1_type, e2_type) with 
                        
                        (* Check if we are assigning custom types. Since we are
                         * past semantic analysis the only possibilities are 1.
                         * we are assining a derived class to its ancestor or 2.
                         * we are assigning the same types. In those cases we
                         * need to cast *) 

                        | (PointerType(CustomType(s), _),
                        PointerType(CustomType(t), _)) ->  (if
                                (Semant.t1_inherits_t2 s t tSymbol_table) then 
                                (CAsnExpr(updated_e1, op,
                                CCastExpr((cType_from_tType tSymbol_table
                                e1_type), updated_e2)), e1_stmts @
                                e2_stmts)
                                
                                else 
                                        (CAsnExpr(updated_e1, op, updated_e2), e1_stmts
                                        @ e2_stmts))
                        | (CustomType(s), CustomType(t)) -> (if
                                (Semant.t1_inherits_t2 s t tSymbol_table) then 
                                (CAsnExpr(updated_e1, op,
                                CCastExpr((cType_from_tType tSymbol_table
                                e1_type), updated_e2)), e1_stmts @
                                e2_stmts)
                                
                                else 
                                        (CAsnExpr(updated_e1, op, updated_e2), e1_stmts
                                        @ e2_stmts))

                        | _ -> (CAsnExpr(updated_e1, op, updated_e2), e1_stmts
                                        @ e2_stmts)
                        )
                   
     | Call(expr, Id(Identifier(s)), expr_list) -> 
                     let sym = Semant.type_from_expr tSymbol_table expr in 
                                        cCallExpr_from_tCallExpr expr
                                        tSymbol_table s expr_list                  
     | MemAccess(expr, Identifier(s)) -> (let typ_ = Semant.type_from_expr
     tSymbol_table expr in match (typ_) with 
                | CustomType(name) -> (CMemAccess(0, fst (update_expr expr
                tSymbol_table ), CIdentifier(s)), [])
                | PointerType(CustomType(name), 1) -> (CMemAccess(1, fst
                (update_expr expr tSymbol_table ), CIdentifier(s)),
                [])
                | _ -> raise(Failure("Bad Mem Access")))
     | Id(Identifier(s)) -> (CId(CIdentifier(s)), [])
     | Literal(d) -> (CLiteral(d), [])
     | FloatLiteral(d) -> (CFloatLiteral(d), [])
     | StringLiteral(s) -> (CStringLiteral(s), [])
     | Postfix(e1, op) -> (let (updated_e1, e1_stmts) = update_expr e1
                                tSymbol_table  in 
                        (CPostfix(updated_e1, op), e1_stmts))
     | _ -> raise(Failure("not finished"))
 
and cExpr_from_tExpr_in_tCall tSymbol_table  tExpr tFuncParam = 
      let expr_type = Semant.type_from_expr
        tSymbol_table tExpr in let param_type = Semant.type_from_func_param
        tFuncParam in  match (expr_type, param_type) with
                        |  (CustomType(a), CustomType(b)) -> if (Semant.is_interface
                        tSymbol_table (Identifier(b))) then CPointify(CMemAccess(0, fst (update_expr tExpr
                        tSymbol_table ),
                        CIdentifier(interface_field_name_in_struct b a))) else ( if
                                (Semant.t1_inherits_t2 a b tSymbol_table) then
                                        CCastExpr(CType(CStruct(cStructName_from_tStruct
                                        b)), fst (update_expr tExpr
                                        tSymbol_table )) else fst (update_expr tExpr
                                        tSymbol_table ))
                        | (PointerType(CustomType(a), 1),
                        CustomType(b)) ->  CPointify(CMemAccess(1, fst (update_expr tExpr
                        tSymbol_table ),
                        CIdentifier(interface_field_name_in_struct b a)))
                        | _ -> fst (update_expr tExpr tSymbol_table
                        )

and cCallExpr_from_tCallExpr expr tSym  func_name expr_list = match expr with
        | Noexpr -> let FuncSymbol(_, fdecl) = StringMap.find func_name tSym in (CCall(0,
        CNoexpr, CId(CIdentifier(func_name)), (List.map2
        (cExpr_from_tExpr_in_tCall tSym ) expr_list fdecl.params)), [])

        | _ -> let expr_type = Semant.type_from_expr tSym expr in (match expr_type with 
                | CustomType(a) -> let fdecl = Semant.get_fdecl_for_receiver a
                func_name tSym in 

                        if (Semant.is_interface tSym (Identifier(a))) then
                                let updated_expr = (fst (update_expr expr tSym))
                                in 
                                let cexpr_list = [CMemAccess(1,
                                (updated_expr), CIdentifier("body"))] @
                                (List.map2 (cExpr_from_tExpr_in_tCall tSym)
                                expr_list fdecl.params) in 

                                (CCall(1, (fst (update_expr expr tSym )),
                                CId(CIdentifier(func_name)), cexpr_list), [])
                        else
                                let first_arg =
                                        CCastExpr(CPointerType(CType(CPrimitiveType(Cvoid)),
                                        1), CPointify(fst (update_expr expr tSym
                                        ))) in 
                                (CCall(0, CNoexpr,
                                CId(CIdentifier(cFunc_from_tMethod a
                                func_name)), [first_arg] @ (List.map2
                                (cExpr_from_tExpr_in_tCall tSym ) expr_list
                                fdecl.params)), [])
               | PointerType(CustomType(a), 1) -> let fdecl =
                       Semant.get_fdecl_for_receiver a func_name tSym in 
                                let first_arg =
                                        CCastExpr(CPointerType(CType(CPrimitiveType(Cvoid)),
                                        1), fst (update_expr expr tSym
                                        )) in 
                                (CCall(0, CNoexpr,
                                CId(CIdentifier(cFunc_from_tMethod a
                                func_name)), [first_arg] @ (List.map2
                                (cExpr_from_tExpr_in_tCall tSym ) expr_list
                                fdecl.params)), [])
               | _ -> raise(Failure("No other functions can call methods")))
 

let c_init_decl_from_string str = 
       CInitDeclarator(CDirectDeclarator(CVar(CIdentifier(str))))

let c_init_decl_from_string_asn str op cExpr = 
       CInitDeclaratorAsn(CDirectDeclarator(CVar(CIdentifier(str))), op, cExpr) 
        

let update_decl decl tSymbol_table  = 
                let id = Semant.var_name_from_declaration decl in 
                   let tType = Semant.type_from_identifier tSymbol_table (Identifier(id))
                   in 
                   
                   match (tType) with 
                   | PrimitiveType(t) -> (let sym = Semant.lookup_symbol_by_id
                   tSymbol_table (Identifier(id)) in (match sym with 
                        | VarSymbol(id, type_) -> (match decl with 
                                        | Declaration(_,
                                        InitDeclList([InitDeclarator(DirectDeclarator(_))]))
                                        -> 
                                        ([CDeclaration(CDeclSpecTypeSpecAny(cType_from_tType
                                        tSymbol_table type_), c_init_decl_from_string id)],
                                        [])
                                        
                                        | Declaration(_,
                                        InitDeclList([InitDeclaratorAsn(_, op,
                                        expr)])) -> let (updated_expr,
                                        extra_stmts) = update_expr expr tSymbol_table
                                         in 
                                                ([CDeclaration(CDeclSpecTypeSpecAny(cType_from_tType
                                                tSymbol_table type_),
                                                c_init_decl_from_string_asn id
                                                op updated_expr)], extra_stmts)
                                                )
                                        )) 
                   | CustomType(t) -> (let sym = Semant.lookup_symbol_by_id
                   tSymbol_table (Identifier(t)) in ( match sym with
                                        | StructSymbol(s, st) -> if
                                                (Semant.get_interface_for_struct t
                                                tSymbol_table
                                       <> "") then (
                                                let interface_ =
                                                        Semant.get_interface
                                                        tSymbol_table
                                                        (Semant.get_interface_for_struct
                                                        t tSymbol_table) in 

                                                let cInterfaceDecl = 
                                                      CDeclaration(CDeclSpecTypeSpecAny(CType(CStruct(cStructName_from_tInterface
                                                interface_.name))),
                                                c_init_decl_from_string (String.concat ""
                                                ["_";id;(cStructName_from_tInterface
                                                interface_.name)])) in

                                                let cdecl = (match decl with
                                                        | Declaration(_,
                                                        InitDeclList([InitDeclarator(DirectDeclarator(_))]))
                                                        ->  
                                                        CDeclaration(CDeclSpecTypeSpecAny(CType(CStruct(cStructName_from_tStruct
                                                        st.struct_name))),
                                                        c_init_decl_from_string
                                                        id)
                                                        
                                                        | _ ->
                                                                        raise(Failure("Need
                                                                        to
                                                                        implement
                                                                        InitDeclAsn")))
                                                in
                                                
                                                let decls = [cInterfaceDecl] @
                                                [cdecl] in 
                                                
                                                (* Assign the C function
                                                        * associated with the
                                                        * method to the
                                                        * corresponding field of
                                                        * the newly created
                                                        * interface struct *) 
                                                let method_asns = List.map (fun
                                                        tmethod_name
                                                -> let
                                                inter_fdecl =
                                                        Semant.get_fdecl_for_receiver
                                                        s tmethod_name
                                                        tSymbol_table in
                                                CExpr(CAsnExpr(CMemAccess(0,
                                                (CId(CIdentifier(String.concat ""
                                                ["_";id;(cStructName_from_tInterface
                                                interface_.name)]))),
                                                (CIdentifier(tmethod_name))),
                                                Asn,
                                                (let cfunc_name =
                                                        cFunc_from_tMethod
                                                        (fst
                                                        (inter_fdecl.receiver)) tmethod_name  in
                                                (CId(CIdentifier(cfunc_name)))))))
                                                (List.map (fun fdecl ->
                                                        Semant.var_name_from_direct_declarator
                                                fdecl.func_name) interface_.funcs) in 

                                                let reference_implementer_asn =
                                                        CExpr(CAsnExpr(CMemAccess(0,
                                                        (CId(CIdentifier(String.concat ""
                                                ["_";id;(cStructName_from_tInterface
                                                interface_.name)]))),
                                                (CIdentifier("body"))), Asn,
                                                CCastExpr(CPointerType(CType(CPrimitiveType(Cvoid)),
                                                1),
                                                CPointify(CId((CIdentifier(id)))))))
                                                in

                                                let
                                                implementer_add_interface_assn = 
                                                        CExpr(CAsnExpr(CMemAccess(0,
                                                        CId(CIdentifier(id)),
                                                        CIdentifier(interface_field_name_in_struct
                                                interface_.name s)), Asn,
                                                CId(CIdentifier(String.concat ""
                                                ["_";id;(cStructName_from_tInterface
                                                interface_.name)])))) in 

                                                let assignments = method_asns @
                                                [reference_implementer_asn] @
                                                [implementer_add_interface_assn]
                                                in

                                                (decls, assignments) 

                                       ) else let cdecl = (match decl with
                                                        | Declaration(_,
                                                        InitDeclList([InitDeclarator(DirectDeclarator(_))]))
                                                        ->  
                                                        CDeclaration(CDeclSpecTypeSpecAny(CType(CStruct(cStructName_from_tStruct
                                                        st.struct_name))),
                                                        c_init_decl_from_string
                                                        id)
                                                        
                                                        | _ ->
                                                                        raise(Failure("Need
                                                                        to
                                                                        implement
                                                                        InitDeclAsn"))
                                       ) in 
                                       ([cdecl], [])
                                        | _ -> raise(Failure("Non struct type
                                        decl'd"))))

               
               

                               
let rec update_statement tstmt tSymbol_table  =  match tstmt with 
        | CompoundStatement(decls, stmts) -> let (new_decls, new_stmts) =
                List.fold_left (fun decl_stmt_acc decl -> let (n_decls, n_stmts)
                = update_decl decl
                tSymbol_table  in ((fst (decl_stmt_acc)) @ n_decls,
                (snd (decl_stmt_acc) @ n_stmts))) ([], []) decls in 
        
                let more_new_stmts = List.fold_left (fun stmt_acc stmt -> let
                (updated_stmt, additional_stmts) = update_statement stmt
                tSymbol_table  in stmt_acc @
                additional_stmts @ [updated_stmt]) [] stmts in 

                (CCompoundStatement(new_decls, new_stmts @ more_new_stmts), [])

        | EmptyElse -> (CEmptyElse, [])
        | Return(e) -> let (updated_e, stmts) = update_expr e tSymbol_table
         in (CReturn(updated_e), stmts)
        | If(e, stmt1, stmt2) -> let (updated_expr, stmts) = update_expr e
        tSymbol_table  in let (updated_stmt1, additional_stmts) = update_statement
        stmt1 tSymbol_table   in let (updated_stmt2,
        additional_stmts2) =
                update_statement stmt2 tSymbol_table  in
        (CIf(updated_expr, updated_stmt1, updated_stmt2), additional_stmts @
        additional_stmts2)
        | For(e1, e2, e3, stmt) -> let (updated_e1, stmts_e1) = update_expr e1
        tSymbol_table  in let (updated_e2, stmts_e2) = update_expr
        e2 tSymbol_table  in let (updated_e3, stmts_e3) =
                update_expr e3 tSymbol_table  in let (updated_stmt,
                additional_stmts) = update_statement stmt  tSymbol_table
                 in (CFor(updated_e1, updated_e2, updated_e3,
                updated_stmt), stmts_e1 @ stmts_e2 @ stmts_e3 @
                additional_stmts)
        | While(e1, stmt) -> let (updated_e1, stmts_e1) = update_expr e1
        tSymbol_table  in let (updated_stmt, additional_stmts) =
                update_statement stmt tSymbol_table  in
        (CWhile(updated_e1, updated_stmt), stmts_e1 @ additional_stmts)
        | Break -> (CBreak, [])
        | Expr(e) -> let (updated_e, stmts) = update_expr e tSymbol_table
         in (CExpr(updated_e), stmts)
        
      
let update_cFunc tSymbol_table cFunc tFunc  =
        let updated_symbol_table = List.fold_left (fun m symbol -> StringMap.add
        (Semant.get_id_from_symbol symbol) symbol m) tSymbol_table ((Semant.symbols_from_decls
        (Semant.get_decls_from_compound_stmt tFunc.body)) @ (Semant.symbols_from_func_params
        tFunc.params) @ ([Semant.symbol_from_receiver tFunc.receiver])) in 

        let CCompoundStatement(decls, stmts) = cFunc.cfunc_body in 
        let CCompoundStatement(updated_decls, updated_stmts) = fst
        (update_statement tFunc.body updated_symbol_table) in
        {
                cfunc_name = cFunc.cfunc_name;
                creturn_type = cFunc.creturn_type;
                cfunc_body = CCompoundStatement(decls @ updated_decls,
                updated_stmts);
                cfunc_params = cFunc.cfunc_params; 
        }

let rec cFunc_from_anonDef symbol_table anonDef =
    let rec convert_anon_params symbol_table params =
        (match params with
            [] -> [(CPointerType(CType(CPrimitiveType(Cvoid)), 1), CIdentifier("capture_struct"))]
          | [p] -> [cFuncParam_from_tFuncParam symbol_table p]
          | h::t -> let htype = (cFuncParam_from_tFuncParam symbol_table h) in
                    let ttype = (convert_anon_params symbol_table t) in
                    [htype]@ttype)
    in {
    cfunc_name = anonDef.anon_name;
    cfunc_body = CCompoundStatement([], []);    
    cfunc_params = (convert_anon_params symbol_table anonDef.anon_params);
    creturn_type = cType_from_tType symbol_table anonDef.anon_return_type
}

and cFunc_list_from_anonDef_list symbol_table adlist = match adlist with
    [] -> []
  | [x] -> [cFunc_from_anonDef symbol_table x]
  | h::t -> let hfuncs = [(cFunc_from_anonDef symbol_table h)] in
            let tfuncs = (cFunc_list_from_anonDef_list symbol_table t) in
            hfuncs@tfuncs

        
(*let rec print_capture_structs symbol_table cs = match cs with*)
    (*[] -> ()*)
  (*| [x] -> ()*)
  (*| h::t -> ()*)

let cProgram_from_tProgram program =
        let updated_program = Semant.update_structs_in_program program in

        let tSymbol_table = Semant.build_symbol_table updated_program in
        Astutil.print_symbol_table tSymbol_table;
        let cstructs_and_functions = List.map (cStruct_from_tStruct tSymbol_table) updated_program.structs in 

        let cstructs = List.map fst cstructs_and_functions in 

        let cfuncs_methods = List.concat (List.map snd cstructs_and_functions) in 

        let cStructs = (List.map (cStruct_from_tInterface
        tSymbol_table) program.interfaces) @ cstructs in

        let tAnonDefs = Semant.anon_defs_from_tprogram program in 
        let cFuncsTranslatedFromAnonDefs = cFunc_list_from_anonDef_list tSymbol_table tAnonDefs in
        (*let capture_structs = capture_struct_list_from_anon_def_list tSymbol_table tAnonDefs in *)
    
        (* The function bodies have not been filled out yet. Just the parameters
         * and return types *)
        let cDeclaredMethodsAndFuncs = cfuncs_methods @ (List.rev (List.map (cFunc_from_tFunc tSymbol_table)
        (List.filter (fun fdecl -> if (fdecl.receiver = ("", "")) then true else
                false) program.functions))) in 
                
        let cUpdatedDeclaredMethodsAndFuncs = List.fold_left (fun acc cFunc -> let sym_ = StringMap.find
        cFunc.cfunc_name tSymbol_table in (match sym_ with 
                       | FuncSymbol(_, fdecl) -> acc @ [update_cFunc tSymbol_table
                       cFunc fdecl] 
                       | _ -> raise(Failure("error")))) [] cDeclaredMethodsAndFuncs in 
       
        let cFuncs = cUpdatedDeclaredMethodsAndFuncs @ cFuncsTranslatedFromAnonDefs
        in


        {
                cstructs = cStructs;
                cglobals = [];
                cfunctions = cFuncs;
        }
