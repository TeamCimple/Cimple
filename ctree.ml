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
  cstruct_name: string;
  cstruct_members: cSymbol list;
  cmethod_to_functions: cFunc StringMap.t;
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
   | CAlloc of cType * cExpr
   | CNeg of cExpr
   | CFree of cExpr
   | CDeref of cExpr
   | CArrayAccess of cExpr * cExpr
   | CCompareExpr of cExpr * tLogicalOperator * cExpr
   | CPointify of cExpr
   | CMemAccess of int * cExpr * cIdentifier (* The int field is a flag to
   indicate it is a pointer dereference *)
   | CId of cIdentifier
   | CDeclExpr of cDeclaration
   | CNoexpr
   | CNull

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

let virtual_table_name_from_tStruct name = 
        String.concat "" ["_virtual";name]

let constructor_name_from_tStruct name = 
        String.concat "_" ["_constructor";name]

let destructor_name_from_tStruct name = 
        String.concat "_" ["_destructor";name]

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

let rec print_pointers n =
        if (n = 1) then "*" else
       ( String.concat "" ["*";(print_pointers (n-1))])

let rec sizeof_string tSymbol_table typ_ = match typ_ with 
  | CType(CPrimitiveType(Cvoid)) -> "void"
  | CType(CPrimitiveType(Cchar)) -> "char"
  | CType(CPrimitiveType(Cint)) -> "int"
  | CType(CPrimitiveType(Clong)) -> "long"
  | CType(CPrimitiveType(Cfloat)) -> "float"
  | CType(CPrimitiveType(Cdouble)) -> "double"
  | CType(CStruct(t)) -> String.concat " " ["struct"; t]
  | CPointerType(base, n) -> String.concat " " [(sizeof_string tSymbol_table
  base); (print_pointers n)] 

let rec cType_from_tType symbol_table = function
    PrimitiveType(typeSpec) -> cType_from_tTypeSpec typeSpec
  | PointerType(base_type, num) -> CPointerType(cType_from_tType symbol_table base_type,
  num)
  | ArrayType(array_type, ptr, e) -> (let t1 = Semant.type_of_array_type
  symbol_table (ArrayType(array_type, ptr, e)) in cType_from_tType symbol_table
  t1)
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
          let captureParam = CPointerType(CType(CPrimitiveType(Cvoid)), 1) in
          CFuncPointer({ 
              func_return_type = anonRetType;
              func_param_types = anonParamTypes@[captureParam]
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
        let generate_void_star_param_types = 
            let anonList = 
                List.filter (fun p -> match p with
                    AnonFuncDecl(anonDecl) -> true
                  | _ -> false)fdecl.params
            in
            let returned_param_types = 
                List.map (fun p -> match p with
                    AnonFuncDecl(anonDecl) ->
                        CPointerType(CType(CPrimitiveType(Cvoid)), 1)) anonList
            in
            returned_param_types
        in
        let extraParamTypes = generate_void_star_param_types in
        let cfunc_return_type =
                cType_from_tType symbol_table (Semant.type_from_declaration_specifiers
                fdecl.return_type) in
        let func_signature = {
                                 func_return_type = cfunc_return_type;
                                 func_param_types = cfunc_param_types@extraParamTypes;
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
                cstruct_members = cBodySymbol @ cSymbols;
                cstruct_name = cStructName_from_tInterface interface.name;
                cmethod_to_functions = StringMap.empty
        }

let cSymbol_from_Implements implements =
        let cstruct_name = cStructName_from_tInterface implements in
        CVarSymbol(cstruct_name, CPointerType(CType(CStruct(cstruct_name)), 1))

let cFuncParam_from_tFuncParam symbol_table tFuncParam =
    (cType_from_tType symbol_table (Semant.type_from_func_param tFuncParam), (CIdentifier(Semant.var_name_from_func_param tFuncParam)))

let create_cfunc_param_for_receiver receiver = 
        (CPointerType(CType(CPrimitiveType(Cvoid)), 1),
        CIdentifier("_body"))

 let create_initial_cast_decl receiver =
        let cstruct_name = cStructName_from_tStruct (fst receiver) in  
        CDeclaration(CDeclSpecTypeSpecAny(CPointerType(CType(CStruct(cstruct_name)),
        1)), CInitDeclaratorAsn(CDirectDeclarator(CVar(CIdentifier(snd receiver)))
        , Asn, CCastExpr(CPointerType(CType(CStruct(cstruct_name)), 1),
        CId(CIdentifier("_body")))))

let number_of_anon_func_parameters_in_tFuncParamList plist = 
    List.fold_left (fun acc f ->
                     (match f with
                        AnonFuncDecl(_) -> (acc + 1)
                      | _ -> acc)) 0 plist

let number_of_anon_func_parameters_in_tFuncDecl fdecl =
    number_of_anon_func_parameters_in_tFuncParamList fdecl.params

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
         * program: an Ast.tProgram.
         * def: The Ast.tAnonFuncDef whose body we are looking through to find captured variables
 * ------------------------------------------------------------------------*)

and capture_struct_from_anon_def program def =
  let symlist = (Semant.symbols_from_outside_scope_for_anon_def program def) in
  let symbols = Semant.symtable_from_symlist symlist in
  let builtinDecls = [{
                       return_type = DeclSpecTypeSpec(Int);
                       func_name = DirectDeclarator(Var(Identifier("printf")));
                       params = [FuncParamsDeclared(DeclSpecTypeSpec(String),
                       DirectDeclarator(Var(Identifier("x"))))];
                       receiver = ("", "");
                       body = CompoundStatement([], [])}] in
  let builtinSyms = Semant.symbols_from_fdecls builtinDecls in
  let rec symconvert m = cSymbol_from_sSymbol symbols m in
  let internal_anon_symbols = (fun stmt -> match stmt with 
                CompoundStatement(declList, _) -> 
                    Semant.symbols_from_decls declList) def.anon_body in
  let param_symbols = (Semant.symbols_from_func_params def.anon_params)@internal_anon_symbols in
  let param_symtable = (Semant.symtable_from_symlist param_symbols) in
  let updated_symbols = Semant.symtable_from_symlist (builtinSyms@symlist) in
    {
      cstruct_name = "S" ^ def.anon_name; (* 's' for 'struct' *)
      cstruct_members = (List.map symconvert (struct_members_from_anon_body updated_symbols param_symtable [] def.anon_body));
      cmethod_to_functions = StringMap.empty
    }


and capture_struct_list_from_anon_def_list program defList = match defList with
      [] -> []
    | [x] -> [capture_struct_from_anon_def program x]
    | h::t -> [capture_struct_from_anon_def program h]@capture_struct_list_from_anon_def_list program t

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
                cstruct_name = cStructName_from_tStruct tStruct.struct_name;
                cstruct_members = cStructMemberSymbols;
                cmethod_to_functions = methods_to_cfunctions;
        }, cfuncs)

let cDeclarationSpecifiers_from_tDeclarationSpecifiers symbol_table tDeclSpecs = function
        | DeclSpecTypeSpecAny(tType) ->
                        CDeclSpecTypeSpecAny(cType_from_tType symbol_table tType)

let cDeclaration_from_tFdecl symbol_table fdecl = 
        let first_argument = [CPointerType(CType(CPrimitiveType(Cvoid)), 1)] in
        let cfunc_param_types = first_argument @ List.map (cType_from_tType symbol_table)
                                     (Semant.type_list_from_func_param_list fdecl.params) in 
        let generate_void_star_param_types = 
            let anonList = 
                List.filter (fun p -> match p with
                    AnonFuncDecl(anonDecl) -> true
                  | _ -> false)fdecl.params
            in
            let returned_param_types = 
                List.map (fun p -> match p with
                    AnonFuncDecl(anonDecl) ->
                        CPointerType(CType(CPrimitiveType(Cvoid)), 1)) anonList
            in
            returned_param_types
        in
        let extraParamTypes = generate_void_star_param_types in
        let cfunc_return_type =
                cType_from_tType symbol_table (Semant.type_from_declaration_specifiers
                fdecl.return_type) in
        let func_signature = {
                                 func_return_type = cfunc_return_type;
                                 func_param_types = cfunc_param_types@extraParamTypes;
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
                cstruct_members = cBodySymbol @ bols;
                cstruct_name = cStructName_from_tInterface interface.name;
                cmethod_to_functions = StringMap.empty
        }

let bol_from_Implements implements struct_name =
        let cstruct_name = cStructName_from_tInterface implements in
        let interface_field_name = interface_field_name_in_struct implements
        struct_name in
        CVarSymbol(interface_field_name, CType(CStruct(cstruct_name)))

let cFuncParam_from_tFuncParam symbol_table tFuncParam =
    match tFuncParam with
       AnonFuncDecl(_) -> 
           let newName = "anon_" ^ (Semant.var_name_from_func_param tFuncParam) in
           (cType_from_tType symbol_table (Semant.type_from_func_param tFuncParam), (CIdentifier(newName)))
     | _ ->
        (cType_from_tType symbol_table (Semant.type_from_func_param tFuncParam), (CIdentifier(Semant.var_name_from_func_param tFuncParam)))

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
    let generate_n_void_star_params n = 
        let anonList = 
            List.filter (fun p -> match p with
                AnonFuncDecl(anonDecl) -> true
              | _ -> false) tFunc.params
        in
        let returned_params = 
            List.map (fun p -> match p with
                AnonFuncDecl(anonDecl) ->
                    (match anonDecl.anon_decl_name with
                        Identifier(s) -> 
                            let paramName = "cap_anon_" ^ s in 
                            (CPointerType(CType(CPrimitiveType(Cvoid)), 1), CIdentifier(paramName)))) anonList
        in
        returned_params
    in
    let n = number_of_anon_func_parameters_in_tFuncDecl tFunc in
    let extraParams = generate_n_void_star_params n in
        {
                creturn_type = cType_from_tType symbol_table
                (Semant.type_from_declaration_specifiers tFunc.return_type);

                cfunc_params = (List.map (cFuncParam_from_tFuncParam
                symbol_table) tFunc.params)@(extraParams);

                cfunc_body = CCompoundStatement([], []);

                cfunc_name = Semant.var_name_from_direct_declarator tFunc.func_name;
        }

let cFunc_from_tMethod tStructName tFuncName =
        Semant.symbol_table_key_for_method tStructName tFuncName

let first_param_for_constructor struct_name =
        (CPointerType(CType(CStruct(cStructName_from_tStruct struct_name)), 2),
        CIdentifier("_this"))

let first_param_for_destructor struct_name =
        (CPointerType(CType(CStruct(cStructName_from_tStruct struct_name)), 2),
        CIdentifier("_this"))

let last_param_for_constructor = 
        (CType(CPrimitiveType(Cint)), CIdentifier("_needs_malloc"))

let last_param_for_destructor = 
        (CType(CPrimitiveType(Cint)), CIdentifier("_needs_free"))


and cFunction_from_tMethod object_type method_ tSymbol_table = 
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
 * This function takes a cimple expression and returns the pair ((C expression,
 * statement[]), cDeclaration[]). The idea is that some cimple expression may generate multiple
 * assignment statements such as a make expression with a constructor.
 * Expressions should not create more declarations, only declarations create
 * more declarations.
 *)
let rec update_expr texpr tSymbol_table tprogram  = match texpr with
        | Neg(e) -> (let ((updated_e1, e1_stmts), decls) = update_expr e
                        tSymbol_table tprogram in ((CNeg(updated_e1), e1_stmts), decls))
        | Binop(e1, op, e2) -> (let ((updated_e1, e1_stmts), _) = update_expr e1
                                tSymbol_table tprogram in 
                        let ((updated_e2, e2_stmts), _) = update_expr e2
                        tSymbol_table  tprogram in 

                        ((CBinop(updated_e1, op, updated_e2), (e1_stmts@e2_stmts)), []))
        | CompareExpr(e1, op, e2) ->
                ( 
                let ((updated_e1, e1_stmts), _) = update_expr e1 tSymbol_table tprogram in 
                let ((updated_e2, e2_stmts), _) = update_expr e2 tSymbol_table tprogram in 
                ((CCompareExpr(updated_e1, op, updated_e2), (e1_stmts @ e2_stmts)), [])
                )
        | Clean(e) -> (let t1 = Semant.type_from_expr tSymbol_table e in 
                      let ((updated_e, e_stmts), decls) = update_expr e tSymbol_table tprogram in 
                      match t1 with 
                      | PointerType(CustomType(s), _) -> ((CCall(0, CNoexpr,
                                CId(CIdentifier(destructor_name_from_tStruct s)),
                                        [CCastExpr(CPointerType(CType(CStruct(cStructName_from_tStruct
                                        s)), 2), CPointify(updated_e))] @
                        [CLiteral(1)]), []), [])

                      | PointerType(_, _) -> ((CFree(updated_e), e_stmts),
                      decls))


        | AsnExpr(e1, op, e2)  ->
                (match e2 with 
                        | Make(typ_, expr_list) -> cAllocExpr_from_tMakeExpr tSymbol_table tprogram e1 e2
                        | _ -> ( 
                            let ((updated_e1, e1_stmts), _) = update_expr e1
                                    tSymbol_table tprogram in 
                            let ((updated_e2, e2_stmts), _) = update_expr e2
                            tSymbol_table tprogram  in 

                            let e1_type = Semant.type_from_expr tSymbol_table e1 in 

                            let e2_type = Semant.type_from_expr tSymbol_table e2 in 

                            (match (e1_type, e2_type) with 
                            
                            (* Check if we are assigning custom types. Since we are
                             * past semantic analysis the only possibilities are 1.
                             * we are assining a derived class to its ancestor or 2.
                             * we are assigning the same types. In those cases we
                             * need to cast *) 

                            | (PointerType(CustomType(s), _),
                            PointerType(CustomType(t), _)) -> 
                                (if (Semant.t1_inherits_t2 s t tSymbol_table) then 
                                    ((CAsnExpr(updated_e1, op,
                                        CCastExpr((cType_from_tType tSymbol_table e1_type), updated_e2)),
                                      (e1_stmts@e2_stmts)), [])
                                    
                                    else 
                                            ((CAsnExpr(updated_e1, op, updated_e2), e1_stmts
                                            @ e2_stmts), []))
                            | (CustomType(s), CustomType(t)) -> (if
                                    (Semant.t1_inherits_t2 s t tSymbol_table) then 
                                    ((CAsnExpr(updated_e1, op,
                                    CCastExpr((cType_from_tType tSymbol_table
                                    e1_type), updated_e2)), (e1_stmts@e2_stmts)), [])
                                    
                                    else 
                                            ((CAsnExpr(updated_e1, op, updated_e2), e1_stmts
                                            @ e2_stmts), []))

                            | _ -> ((CAsnExpr(updated_e1, op, updated_e2), e1_stmts
                            @ e2_stmts), []))))
                   
     | Call(expr, Id(Identifier(s)), expr_list) -> 
                     let sym = Semant.type_from_expr tSymbol_table expr in 
                     let ret = cCallExpr_from_tCallExpr expr tSymbol_table tprogram s expr_list in
                     ret
     | Super(_) -> ((CNoexpr, []), [])
     | ArrayAccess(e1, e2) -> ((CArrayAccess(fst(fst(update_expr e1 tSymbol_table
     tprogram)), fst(fst(update_expr e2 tSymbol_table tprogram))), []), []) 
     | MemAccess(expr, Identifier(s)) -> (let typ_ = Semant.type_from_expr
     tSymbol_table expr in match (typ_) with 
                | CustomType(name) -> ((CMemAccess(0, fst( fst (update_expr expr
                tSymbol_table tprogram )), CIdentifier(s)), []), [])
                | PointerType(CustomType(name), 1) -> ((CMemAccess(1, fst(fst
                (update_expr expr tSymbol_table tprogram )), CIdentifier(s)),
                []), [])
                | _ -> raise(Failure("Bad Mem Access")))
     | Id(Identifier(s)) -> ((CId(CIdentifier(s)), []), [])
     | Literal(d) -> ((CLiteral(d), []), [])
     | Make(typ_, expr_list) -> (let ctype = cType_from_tType tSymbol_table typ_
                  in match typ_ with 
                 | PrimitiveType(s) -> ((CAlloc(ctype,
                 CId(CIdentifier((sizeof_string tSymbol_table ctype)))), []), [])
                 | ArrayType(array_type, ptr, e) ->(let updated_e = fst(fst(update_expr
                        e tSymbol_table tprogram)) in let pointer_type = Semant.type_of_array_type
       tSymbol_table typ_ in let cpointer_type = cType_from_tType tSymbol_table
       pointer_type in (match cpointer_type with 
       | CPointerType(base, num) ->let ctype_to_malloc = if (num = 1) then base
       else CPointerType(base, num-1) in (
       CAlloc(base, CBinop(updated_e, Mul,
       (CId(CIdentifier(sizeof_string tSymbol_table ctype_to_malloc))))), []),
       []))
                 
                 | CustomType(s) -> ((CAlloc(ctype, CId(CIdentifier(sizeof_string tSymbol_table
                ctype))), []), []))
     | FloatLiteral(d) -> ((CFloatLiteral(d), []), [])
     | StringLiteral(s) -> ((CStringLiteral(s), []), [])
     | Postfix(e1, op) -> (let ((updated_e1, e1_stmts), _) = update_expr e1
                                tSymbol_table  tprogram in ((CPostfix(updated_e1, op), e1_stmts), []))
     | AnonFuncDef(anonDef) ->
             let anon_name = Semant.find_name_for_anon_def tprogram anonDef in
             let instanceName = "s" ^ anon_name in 
             let structName = "S" ^ anon_name in
             let decls = [CDeclaration(CDeclSpecTypeSpecAny(CType(CStruct(structName))), CInitDeclarator(CDirectDeclarator(CVar(CIdentifier(instanceName)))))] in
             let assignments_from_capture_struct c = 
                 List.map (fun csym -> 
                    (match csym with
                        CVarSymbol(s, _) ->
                            CExpr(CAsnExpr(CMemAccess(0, CId(CIdentifier(instanceName)), CIdentifier(s)), Asn, CId(CIdentifier(s))))
                      | _ -> raise(Failure("update_expr: Invalid CSymbol parameter")))) c.cstruct_members
             in

             let captures = capture_struct_from_anon_def tprogram anonDef in

             let newAssignments = assignments_from_capture_struct captures in
             ((CId(CIdentifier(anon_name)), newAssignments), decls)
     | Noexpr -> ((CNoexpr, []), [])
     | Pointify(e) -> let ((updated_e, stmts), decls) = update_expr e
     tSymbol_table tprogram in ((CPointify(updated_e), stmts), decls)
     | Deref(e) -> let ((updated_e, stmts), decls) = update_expr e tSymbol_table
     tprogram in ((CDeref(updated_e), stmts), decls)
     | Nil -> ((CNull, []), [])
     | _ ->
             let expr_type = Astutil.string_of_expr texpr in 
             raise(Failure("not finished for type " ^ expr_type))

and update_expr_list texpr_list tSymbol_table tprogram = match texpr_list with
    [] -> []
  | [e] -> [update_expr e tSymbol_table tprogram]
  | h::t -> [update_expr h tSymbol_table tprogram]@update_expr_list t tSymbol_table tprogram

and generate_stmts_for_parent_destructor symbol_table tprogram destructor
tStruct = 

        let ancestor_destructor = Semant.get_ancestors_destructor symbol_table
        tStruct in 

        let c_ancestor_destructor_name = destructor_name_from_tStruct
        ancestor_destructor.destructor_name in 

        let first_arg = CCastExpr(CPointerType(cType_from_tType symbol_table
        (CustomType(ancestor_destructor.destructor_name)), 2),
        CId(CIdentifier("_this"))) in 

        let last_arg = CLiteral(0) in 

        [(CExpr(CCall(0, CNoexpr,
        CId(CIdentifier(c_ancestor_destructor_name)), [first_arg]@[last_arg])))]

and generate_stmts_for_super expr_list symbol_table tprogram constructor tStruct =
        let ancestor_constructor = Semant.get_ancestors_constructor
        symbol_table tStruct in 

        let c_ancestor_constructor_name = constructor_name_from_tStruct
        ancestor_constructor.constructor_name in 

        let first_arg = CCastExpr(CPointerType(cType_from_tType symbol_table
        (CustomType(ancestor_constructor.constructor_name)), 2), CId(CIdentifier("_this")))
        in 

        let last_arg = CLiteral(0) in 
        
        let call_to_super_constructor_stmt = [(CExpr(CCall(0, CNoexpr, CId(CIdentifier(
                c_ancestor_constructor_name)), [first_arg]@(List.map2
                                (cExpr_from_tExpr_in_tCall symbol_table tprogram) expr_list
                                ancestor_constructor.constructor_params)@[last_arg])))] in 

        let StructSymbol(_, ancestor_struct) = Semant.lookup_symbol_by_id
        symbol_table
        (Identifier(ancestor_constructor.constructor_name)) in


        let local_reassignment_of_members = List.fold_left (fun assignments
        member -> let member_id = Semant.var_name_from_declaration member
        in assignments @ [CExpr(CAsnExpr(CId(CIdentifier(member_id)), Asn, CMemAccess(1,
        CDeref(CId(CIdentifier("_this"))),
        (CIdentifier(member_id)))))]) [] ancestor_struct.members in 

        call_to_super_constructor_stmt @ local_reassignment_of_members


and cAllocExpr_from_tMakeExpr tSymbol_table tprogram asn_expr tMakeExpr = 
        let ((updated_e1, updated_stmts), _) = (update_expr asn_expr tSymbol_table tprogram ) in
        let Make(typ_, expr_list) = tMakeExpr in
        let ctype = cType_from_tType tSymbol_table typ_ in  
        match typ_ with 
        | CustomType(typ) -> (
                let StructSymbol(name, tStruct) = Semant.lookup_symbol_by_id
                tSymbol_table (Identifier(typ)) in 

                if (tStruct.constructor.constructor_name <> "") then (
                        (* We have a constructor *)
                        let params = tStruct.constructor.constructor_params in 
                        
                        let more_params_filtered = generate_extra_capture_func_params_from_expr_list tSymbol_table tprogram expr_list in 
                        let updated_expr_list = (List.map2
                        (cExpr_from_tExpr_in_tCall tSymbol_table tprogram) expr_list params) in 
                    
                        let anonParams = Semant.anon_defs_from_expr_list_no_recursion tprogram expr_list in
                        let update_anon_def_expr_list anonList = 
                            List.fold_left (fun ((e, slist), dlist) def ->
                                let ((_, _slist), _dlist) = update_expr (AnonFuncDef(def)) tSymbol_table tprogram in
                                ((Noexpr, slist@_slist), dlist@_dlist)) ((Noexpr, []), []) anonList
                        in 
                        let ((updated_expr, updated_slist), updated_dlist) = update_anon_def_expr_list anonParams in
                        
                       ((CCall(0, CNoexpr,
                        CId(CIdentifier(constructor_name_from_tStruct
                        tStruct.struct_name)),
                        [CCastExpr(CPointerType(CType(CStruct(cStructName_from_tStruct
                        tStruct.struct_name)), 2), CPointify(updated_e1))] @
                        updated_expr_list @ [CLiteral(1)]@more_params_filtered), updated_slist), updated_dlist)
                ) else (
                       ((CCall(0, CNoexpr,
                        CId(CIdentifier(constructor_name_from_tStruct
                        tStruct.struct_name)),
                        [CCastExpr(CPointerType(CType(CStruct(cStructName_from_tStruct
                        tStruct.struct_name)), 2), CPointify(updated_e1))] @
                        [CLiteral(1)]), []), [])
               ))
       | (ArrayType(array_type, ptr, e)) -> (let updated_e = fst(fst(update_expr
                        e tSymbol_table tprogram)) in let pointer_type = Semant.type_of_array_type
       tSymbol_table typ_ in let cpointer_type = cType_from_tType tSymbol_table
       pointer_type in (match cpointer_type with 
       | CPointerType(base, num) ->let ctype_to_malloc = if (num = 1) then base
       else CPointerType(base, num-1) in ((CAsnExpr(updated_e1, Asn,
       CAlloc(base, CBinop(updated_e, Mul,
       (CId(CIdentifier(sizeof_string tSymbol_table ctype_to_malloc)))))), []),
       [])))
            
   
and cExpr_from_tExpr_in_tCall tSymbol_table  tprogram  tExpr tFuncParam = 
      let expr_type = Semant.type_from_expr tSymbol_table tExpr in 
      let param_type = Semant.type_from_func_param tFuncParam in 
        match (expr_type, param_type) with
            |  (CustomType(a), CustomType(b)) -> 
                    if (Semant.is_interface tSymbol_table (Identifier(b))) then
                        CPointify(CMemAccess(0, fst( fst (update_expr tExpr tSymbol_table  tprogram )),
                            CIdentifier(interface_field_name_in_struct b a))) 
                    else ( if (Semant.t1_inherits_t2 a b tSymbol_table) then
                        CCastExpr(CType(CStruct(cStructName_from_tStruct b)),
                                        fst (fst (update_expr tExpr tSymbol_table  tprogram ))) 
                    else fst (fst (update_expr tExpr tSymbol_table  tprogram )))
            | (PointerType(CustomType(a), 1),
            CustomType(b)) ->  CPointify(CMemAccess(1, fst( fst (update_expr tExpr
            tSymbol_table  tprogram )),
            CIdentifier(interface_field_name_in_struct b a)))
            | _ -> fst (fst (update_expr tExpr tSymbol_table tprogram )
            )

and generate_extra_capture_func_params_from_expr_list tSym tprogram expr_list = 
        let anonParams = Semant.anon_defs_from_expr_list_no_recursion tprogram expr_list in
        let update_anon_def_expr_list anonList = 
            List.fold_left (fun ((e, slist), dlist) def ->
                let ((_, _slist), _dlist) = update_expr (AnonFuncDef(def)) tSym tprogram in
                ((Noexpr, slist@_slist), dlist@_dlist)) ((Noexpr, []), []) anonList
        in 
        let capture_struct_instance_name_from_anon_def def = 
            let capStruct = capture_struct_from_anon_def tprogram def in
            let subname = String.sub capStruct.cstruct_name 1 ((String.length capStruct.cstruct_name) - 1) in
            let structname = "s" ^ subname in
            structname
        in
        let capture_params_from_anon_def_list defList = 
            List.fold_left (fun elist def -> 
                elist@[CPointify(CId(CIdentifier((capture_struct_instance_name_from_anon_def def))))]
            ) [CNoexpr] defList
        in
        let more_params = capture_params_from_anon_def_list anonParams in
        let remove_noexpr_from_list elist = 
            List.filter (fun e ->
                match e with
                    CNoexpr -> false
                  | _ -> true) elist
        in
        let more_params_filtered = remove_noexpr_from_list more_params in 
        more_params_filtered

and cCallExpr_from_tCallExpr expr tSym  tprogram func_name expr_list =
    match expr with
        | Noexpr -> let sym = StringMap.find func_name tSym in 
                    (match sym with
                        FuncSymbol(_, fdecl) -> 
                            let hasAnonParams = Semant.expr_list_contains_anon_defs_no_recursion expr_list in 
                            if (hasAnonParams = true) then 
                                let update_anon_def_expr_list anonList = 
                                    List.fold_left (fun ((e, slist), dlist) def ->
                                        let ((_, _slist), _dlist) = update_expr (AnonFuncDef(def)) tSym tprogram in
                                        ((Noexpr, slist@_slist), dlist@_dlist)) ((Noexpr, []), []) anonList
                                in 
                                let more_params_filtered = generate_extra_capture_func_params_from_expr_list tSym tprogram expr_list in 
                                let anonParams = Semant.anon_defs_from_expr_list_no_recursion tprogram expr_list in
                                let ((updated_expr, updated_slist), updated_dlist) = update_anon_def_expr_list anonParams in
                                let paramExpressions = (List.map2
                                    (cExpr_from_tExpr_in_tCall tSym  tprogram ) expr_list fdecl.params) in
                               
                                let ret =   
                                ((CCall(0, CNoexpr, CId(CIdentifier(func_name)), paramExpressions@more_params_filtered) , updated_slist), updated_dlist)
                                in
                                ret;
                            else
                                if (func_name = "printf") then
                                    let expr_list = (List.rev expr_list) in
                                    let replacementParamList = Semant.func_param_list_from_expr_list tSym expr_list in
                                    ((CCall(0, CNoexpr, CId(CIdentifier(func_name)), (List.map2
                                        (cExpr_from_tExpr_in_tCall tSym  tprogram ) expr_list replacementParamList)), []), [])
                                else
                                    let ret =
                                    ((CCall(0, CNoexpr, CId(CIdentifier(func_name)), (List.map2
                                        (cExpr_from_tExpr_in_tCall tSym  tprogram ) expr_list fdecl.params)), []), [])
                                    in
                                    ret
                      | AnonFuncSymbol(anonName, AnonFuncType(_, tlist)) ->
                              let rec funcParam_from_tType t = (match t with
                                    _ -> ParamDeclWithType(DeclSpecTypeSpecAny(t)))
                              
                                and funcParamList_from_tTypeList tlist = (match tlist with
                                    [] -> []
                                  | [x] -> [funcParam_from_tType x]
                                  | h::t -> [funcParam_from_tType h]@(funcParamList_from_tTypeList t))
                              in
                              let fParamList = funcParamList_from_tTypeList tlist in
                              let fParamExprList = (List.map2 (cExpr_from_tExpr_in_tCall tSym tprogram ) expr_list fParamList) in
                              let extraParamName = "cap_anon_" ^ func_name in
                              let extraParamExpr = CId(CIdentifier(extraParamName)) in
                              ((CCall(1, CNoexpr, CId(CIdentifier("anon_" ^ func_name)), fParamExprList@[extraParamExpr]), []), []))

        | _ -> let expr_type = Semant.type_from_expr tSym expr in (match expr_type with 
                | CustomType(a) -> let fdecl = Semant.get_fdecl_for_receiver a
                tSym func_name in 

                        if (Semant.is_interface tSym (Identifier(a))) then
                                let updated_expr = (fst (fst (update_expr expr tSym tprogram )))
                                in 
                                let cexpr_list = [CMemAccess(1,
                                (updated_expr), CIdentifier("body"))] @
                                (List.map2 (cExpr_from_tExpr_in_tCall tSym tprogram )
                                expr_list fdecl.params) in 

                                ((CCall(1, (fst (fst (update_expr expr
                                tSym tprogram))),
                                CId(CIdentifier(func_name)), cexpr_list), []),
                                [])
                        else
                            let hasAnonParams = Semant.expr_list_contains_anon_defs_no_recursion expr_list in 
                            if (hasAnonParams = true) then 
                                let update_anon_def_expr_list anonList = 
                                    List.fold_left (fun ((e, slist), dlist) def ->
                                        let ((_, _slist), _dlist) = update_expr (AnonFuncDef(def)) tSym tprogram in
                                        ((Noexpr, slist@_slist), dlist@_dlist)) ((Noexpr, []), []) anonList
                                in 
                                let extra_params = generate_extra_capture_func_params_from_expr_list tSym tprogram expr_list in 
                                let anonParams = Semant.anon_defs_from_expr_list_no_recursion tprogram expr_list in
                                let ((updated_expr, updated_slist), updated_dlist) = update_anon_def_expr_list anonParams in
                                let first_arg =
                                        CCastExpr(CPointerType(CType(CPrimitiveType(Cvoid)),
                                        1), CPointify(fst(fst (update_expr expr
                                        tSym tprogram
                                        )))) in 
                                ((CCall(1, CMemAccess(0, fst (fst( (update_expr expr
                                tSym tprogram))), CIdentifier("_virtual")), 
                                CId(CIdentifier(
                                func_name)), [first_arg] @ (List.map2
                                (cExpr_from_tExpr_in_tCall tSym  tprogram ) expr_list
                                fdecl.params)@extra_params), updated_slist), updated_dlist)
                            else
                                let first_arg =
                                        CCastExpr(CPointerType(CType(CPrimitiveType(Cvoid)),
                                        1), CPointify(fst(fst (update_expr expr
                                        tSym tprogram
                                        )))) in 
                                ((CCall(1, CMemAccess(0, fst (fst( (update_expr expr
                                tSym tprogram))), CIdentifier("_virtual")), 
                                CId(CIdentifier(
                                func_name)), [first_arg] @ (List.map2
                                (cExpr_from_tExpr_in_tCall tSym  tprogram ) expr_list
                                fdecl.params)),[]), [])
               | PointerType(CustomType(a), 1) ->
                       (*Printf.printf "Pointer Type expr list is %s\n" (Astutil.string_of_expr_list expr_list);*)
                       let hasAnonParams = Semant.expr_list_contains_anon_defs_no_recursion expr_list in 
                       if (hasAnonParams = true) then 
                           let fdecl =
                           Semant.get_fdecl_for_receiver a tSym func_name in
                                    let first_arg =
                                            CCastExpr(CPointerType(CType(CPrimitiveType(Cvoid)),
                                            1), fst (fst (update_expr expr tSym
                                            tprogram
                                            ))) in 
                           let update_anon_def_expr_list anonList = 
                               List.fold_left (fun ((e, slist), dlist) def ->
                                   let ((_, _slist), _dlist) = update_expr (AnonFuncDef(def)) tSym tprogram in
                                   ((Noexpr, slist@_slist), dlist@_dlist)) ((Noexpr, []), []) anonList
                           in 
                           let extra_params = generate_extra_capture_func_params_from_expr_list tSym tprogram expr_list in 
                           let anonParams = Semant.anon_defs_from_expr_list_no_recursion tprogram expr_list in
                           let ((updated_expr, updated_slist), updated_dlist) = update_anon_def_expr_list anonParams in
                            ((CCall(1, CMemAccess(1,
                            fst (fst(update_expr expr tSym tprogram)),
                            CIdentifier("_virtual")), 
                           CId(CIdentifier(
                            func_name)), [first_arg] @ (List.map2
                            (cExpr_from_tExpr_in_tCall tSym  tprogram ) expr_list
                            fdecl.params@extra_params)), updated_slist), updated_dlist)
                       else 
                           let fdecl =
                           Semant.get_fdecl_for_receiver a tSym func_name in
                                (*Printf.printf "We have interface!\n";*)
                                    let first_arg =
                                            CCastExpr(CPointerType(CType(CPrimitiveType(Cvoid)),
                                            1), fst (fst (update_expr expr tSym
                                            tprogram
                                            ))) in 
                                    let extra_params = generate_extra_capture_func_params_from_expr_list tSym tprogram expr_list in 

                                    ((CCall(1, CMemAccess(1,
                                    fst (fst(update_expr expr tSym tprogram)),
                                    CIdentifier("_virtual")), 
                                   CId(CIdentifier(
                                    func_name)), [first_arg] @ (List.map2
                                    (cExpr_from_tExpr_in_tCall tSym  tprogram ) expr_list
                                    fdecl.params)), []), [])
               | _ -> raise(Failure("No other functions can call methods")))

let generate_virtual_table_assignments isPointer tStruct tSymbol_table id= 
        let fdecls = Semant.get_unique_method_names_for_struct tSymbol_table tStruct in

       
         List.map (fun tmethod_name -> 
                                       
                let inter_fdecl = Semant.get_fdecl_for_receiver 
                                           tStruct.struct_name tSymbol_table
                                           tmethod_name in

                let cFunc_name = cFunc_from_tMethod (fst(inter_fdecl.receiver)) tmethod_name in

                if (isPointer >= 0) then 
                CExpr(CAsnExpr(CMemAccess(isPointer,CId(CIdentifier(id)), CIdentifier(tmethod_name)),
                      Asn,
                     (CId(CIdentifier(cFunc_name)))))
                else
                CExpr(CAsnExpr(CMemAccess(1,CDeref(CId(CIdentifier(id))), CIdentifier(tmethod_name)),
                      Asn,
                     (CId(CIdentifier(cFunc_name)))))) 
         fdecls
                                 

let c_init_decl_from_string str = 
       CInitDeclarator(CDirectDeclarator(CVar(CIdentifier(str))))

let c_init_decl_from_string_asn str op cExpr = 
       CInitDeclaratorAsn(CDirectDeclarator(CVar(CIdentifier(str))), op, cExpr) 
        

let generate_decls_and_stmts_from_id tSymbol_table tprogram id decl typ_ = 
       match decl with 
       | Declaration(_, InitDeclList([InitDeclarator(_)])) ->
                       let ctype = cType_from_tType tSymbol_table typ_ in 
                       let cinit_decl = c_init_decl_from_string id in 
                       ([CDeclaration(CDeclSpecTypeSpecAny(ctype),
                       cinit_decl)], [])

       | Declaration(_, InitDeclList([InitDeclaratorAsn(_, op, expr)])) -> 
                       let ((updated_expr, extra_stmts), _) = update_expr expr
                       tSymbol_table  tprogram in

                       let ctype = cType_from_tType tSymbol_table typ_ in
                       let cinit_decl = c_init_decl_from_string_asn id op
                       updated_expr in 

                       ([CDeclaration(CDeclSpecTypeSpecAny(ctype),
                       cinit_decl)], extra_stmts)


let update_decl_for_non_custom_type id decl tSymbol_table  tprogram = 
        let sym = Semant.lookup_symbol_by_id tSymbol_table (Identifier(id)) in (match
        sym with 
        | VarSymbol(id, type_) -> (match decl with 
                        | Declaration(_, InitDeclList([InitDeclarator(DirectDeclarator(_))])) ->
                                generate_decls_and_stmts_from_id tSymbol_table tprogram 
                                id decl type_
                        | Declaration(_, InitDeclarator(DirectDeclarator(_))) ->
                                generate_decls_and_stmts_from_id tSymbol_table tprogram 
                                id decl type_
                        | Declaration(_, InitDeclaratorAsn(declrt, _, _)) -> 
                                generate_decls_and_stmts_from_id tSymbol_table tprogram 
                                id decl type_
                        | _ -> 
                                generate_decls_and_stmts_from_id tSymbol_table tprogram 
                                id decl type_)
                        (*| _ -> raise(Failure("cannot update decl for other symbol right now: " ^*)
                        (*id)))*)
        | FuncSymbol(_, _) -> raise(Failure("update_decl: FuncSymbol not supported"))
        | AnonFuncSymbol(_, _) -> raise(Failure("update_decl: AnonFuncSymbol not supported"))
        ) 

let declare_virtual_table_stack tSymbol_table tStruct id =
        let virtual_table_name = virtual_table_name_from_tStruct
        tStruct.struct_name in

        let virtual_table_type =
                CType(CStruct(virtual_table_name)) in
        
        let virtual_table_id = String.concat "" ["_";id;virtual_table_name] in 

        let virtual_table_init_decl = c_init_decl_from_string (String.concat ""
        ["_"; id; (virtual_table_name)]) in 

        let virtual_table_assignments = generate_virtual_table_assignments 0 tStruct tSymbol_table
        virtual_table_id in

        let assign_virtual_table_back_to_id = CExpr(CAsnExpr(CMemAccess(0,
        CId(CIdentifier(id)), 
        CIdentifier("_virtual")), Asn,
        CPointify(CId(CIdentifier(virtual_table_id))))) in 

        ([CDeclaration(CDeclSpecTypeSpecAny(CType(CStruct(virtual_table_name))),
        virtual_table_init_decl)], virtual_table_assignments @
        [assign_virtual_table_back_to_id])

let update_interface_decl_for_struct id struct_name tSymbol_table = 
        let interface = Semant.get_interface tSymbol_table
        (Semant.get_interface_for_struct struct_name tSymbol_table) in

        let cstruct_name_for_interface = cStructName_from_tInterface
        interface.name in

        let cinit_decl = c_init_decl_from_string (String.concat ""
        ["_";id;(cStructName_from_tInterface interface.name)]) in 

        CDeclaration(CDeclSpecTypeSpecAny(CType(CStruct(cstruct_name_for_interface))),
                                               cinit_decl)

let interface_decl_and_assignments_for_struct isPointer struct_ tSymbol_table id
= 
        let custom_type = struct_.struct_name in
        
        let implements = Semant.get_interface_for_struct custom_type tSymbol_table in

        if (implements <> "") then (
                   let interface = Semant.get_interface tSymbol_table
                                   (Semant.get_interface_for_struct
                                    custom_type tSymbol_table) in

                   let interface_decl = update_interface_decl_for_struct
                        id custom_type tSymbol_table in

                   let cstruct_for_interface = cStructName_from_tInterface
                                        implements in

                   let access_id = String.concat "" ["_";id;cstruct_for_interface] in 

                   let fdecls = (List.map (fun fdecl -> Semant.var_name_from_direct_declarator
                                                fdecl.func_name) interface.funcs) in
                   let interface_assignments  = List.map (fun tmethod_name -> 
                                   let inter_fdecl =  Semant.get_fdecl_for_receiver 
                                                custom_type tSymbol_table
                                                tmethod_name in

                                   let cFunc_name = cFunc_from_tMethod (fst
                                        (inter_fdecl.receiver)) tmethod_name in

                                        
                                   CExpr(CAsnExpr(CMemAccess(0,CId(CIdentifier(access_id)),
                                        CIdentifier(tmethod_name)),
                                                Asn,
                                                (CId(CIdentifier(cFunc_name)))))
                                        ) fdecls in
                                
                                 
                    let reference_implementer_asn =
                              let struct_expr = if (isPointer = 0) then
                                      CPointify(CId(CIdentifier(id))) else (if
                                              (isPointer > 0) then 
                                              (CId(CIdentifier(id)))
                                      else 
                                              (CDeref(CId(CIdentifier(id))))) in

                                        CExpr(CAsnExpr(CMemAccess(0,
                                        CId(CIdentifier(access_id)),
                                        CIdentifier("body")), Asn,
                                        CCastExpr(CPointerType(CType(CPrimitiveType(Cvoid)),
                                        1), struct_expr))) in
                                
                    let interface_field_name = interface_field_name_in_struct implements custom_type in

                    let cStruct_mem_access_expr = if (isPointer >= 0) then CMemAccess(isPointer,
                                                CId(CIdentifier(id)),
                                                CIdentifier(interface_field_name))
                    else (CMemAccess(1, CDeref(CId(CIdentifier(id))),
                    CIdentifier(interface_field_name))) in 
                                 
                    let implementer_add_interface_assn = 
                                                        CExpr(CAsnExpr(cStruct_mem_access_expr,
                                                        Asn,
                                                        CId(CIdentifier(access_id)))) in 

                    let assignments = interface_assignments @
                                                [reference_implementer_asn] @
                                                [implementer_add_interface_assn]
                                                in

                    ([interface_decl],
                                 assignments) 
 
        ) else ([], [])

let update_decl_for_custom_type id decl custom_type tSymbol_table  tprogram = 
        let sym = Semant.lookup_symbol_by_id tSymbol_table
        (Identifier(custom_type)) in 

        match sym with 
        | StructSymbol(s, struct_) -> 
                        let cstruct_name = cStructName_from_tStruct custom_type
                        in 

                        let cstruct_type = CustomType(custom_type) in 

                        let (cstruct_decl, stmts) = generate_decls_and_stmts_from_id
                        tSymbol_table  tprogram id decl cstruct_type in

                        let (virtual_table_decl, assignments) =
                                declare_virtual_table_stack tSymbol_table
                                struct_ id in 

                        let (decls, assigns) =
                                interface_decl_and_assignments_for_struct 0
                                struct_ tSymbol_table id in

                        (cstruct_decl@decls@virtual_table_decl,
                        stmts@assigns@assignments)
                       

let update_decl decl tSymbol_table  tprogram  = 
                let id = Semant.var_name_from_declaration decl in 
                   let tType = Semant.type_from_identifier tSymbol_table (Identifier(id))
                   in 
                   
                   match (tType) with
                   | PrimitiveType(t) -> update_decl_for_non_custom_type id decl
                   tSymbol_table tprogram 
                   | CustomType(t) -> update_decl_for_custom_type id decl t
                   tSymbol_table tprogram 
                   | _ -> generate_decls_and_stmts_from_id tSymbol_table  tprogram id decl tType
                                     
                               
let rec update_statement tstmt tSymbol_table  tprogram  =  match tstmt with 
        | CompoundStatement(decls, stmts) ->
                let updated_symbol_table = Semant.add_to_symbol_table tSymbol_table decls in  
                let (new_decls, new_stmts) = 
                    List.fold_left (fun decl_stmt_acc decl ->
                        let (n_decls, n_stmts) = update_decl decl
                        updated_symbol_table tprogram in
                        ((fst (decl_stmt_acc)) @ n_decls, (snd (decl_stmt_acc) @ n_stmts))) ([], []) decls in 
       
                let more_new_stmts = 
                    List.fold_left (fun stmt_acc stmt -> 
                                           let ((updated_stmt, additional_stmts), additional_decls) =
                                               update_statement stmt
                                               updated_symbol_table  tprogram in
                                           stmt_acc @ additional_stmts @ [updated_stmt]) [] stmts in 
                let more_new_decls = 
                    List.fold_left (fun decl_acc stmt -> 
                                           let ((updated_stmt, additional_stmts), additional_decls) =
                                               update_statement stmt
                                               updated_symbol_table  tprogram in
                                           decl_acc @ additional_decls) [] stmts in 

                ((CCompoundStatement(new_decls@more_new_decls, new_stmts @ more_new_stmts), []), [])

        | EmptyElse -> ((CEmptyElse, []), [])
        | Return(e) -> let ((updated_e, stmts), newDecls) = update_expr e tSymbol_table tprogram 
         in ((CReturn(updated_e), stmts), newDecls)
        | If(e, stmt1, stmt2) -> 
                let ((updated_expr, stmts), decls) = update_expr e tSymbol_table  tprogram  in 
                let ((updated_stmt1, additional_stmts), additional_decls) = update_statement stmt1 tSymbol_table  tprogram in
                let ((updated_stmt2, additional_stmts2), additional_decls2) = update_statement stmt2 tSymbol_table  tprogram  in
        ((CIf(updated_expr, updated_stmt1, updated_stmt2), additional_stmts@additional_stmts2), [])
        | For(e1, e2, e3, stmt) -> let ((updated_e1, stmts_e1), decls_e1) = update_expr e1 tSymbol_table  tprogram in 
                                   let ((updated_e2, stmts_e2), decls_e2) = update_expr e2 tSymbol_table  tprogram in
                                   let ((updated_e3, stmts_e3), decls_e3) = update_expr e3 tSymbol_table  tprogram in
                                   let ((updated_stmt, additional_stmts), decls_stmt) = update_statement stmt  tSymbol_table  tprogram in 
                                   let accumulated_stmts = (stmts_e1@stmts_e2@stmts_e3@additional_stmts) in
                                   let accumulated_decls = (decls_e1@decls_e2@decls_e3@decls_stmt) in
                                   ((CFor(updated_e1, updated_e2, updated_e3, updated_stmt), accumulated_stmts), accumulated_decls)
        | While(e1, stmt) -> 
                let ((updated_e1, stmts_e1), decls_e1) = update_expr e1 tSymbol_table  tprogram in
                let ((updated_stmt, additional_stmts), additional_decls) = update_statement stmt tSymbol_table tprogram in
                ((CWhile(updated_e1, updated_stmt), stmts_e1 @ additional_stmts), (decls_e1@additional_decls)) 
        | Break -> ((CBreak, []), [])
        | Expr(e) -> let ((updated_e, stmts), decls) = update_expr e tSymbol_table  tprogram in
                     ((CExpr(updated_e), stmts), decls)

let cFunc_from_tDestructor symbol_table tprogram destructor tStruct = 
        let cdestructor_name = destructor_name_from_tStruct
        tStruct.struct_name in 

        let first_param = first_param_for_destructor tStruct.struct_name in 

        let last_param = last_param_for_destructor in 

        let augmented_decls = List.fold_left (fun cdecls tdecl ->
                   let tdecl_id = (Semant.var_name_from_declaration tdecl) in
                   let tdecl_type = (Semant.type_from_declaration tdecl) in 

                   let (cdecl, _)  = generate_decls_and_stmts_from_id symbol_table tprogram 
                   tdecl_id tdecl tdecl_type in cdecls @ cdecl
        ) [] tStruct.members in 

        {
                creturn_type = CType(CPrimitiveType(Cvoid));
                                                
                cfunc_params = [first_param] @ [last_param];

                cfunc_body = CCompoundStatement(augmented_decls,
                []);
        
                cfunc_name = cdestructor_name;
        }

let cFunc_from_tConstructor symbol_table  tprogram constructor tStruct = 
        let cconstructor_name = constructor_name_from_tStruct
        tStruct.struct_name
        in 
        
        let first_param = first_param_for_constructor tStruct.struct_name in

        let last_param = last_param_for_constructor in 

        let augmented_decls = List.fold_left (fun cdecls tdecl ->
                   let tdecl_id = (Semant.var_name_from_declaration tdecl) in
                   let tdecl_type = (Semant.type_from_declaration tdecl) in 

                   let (cdecl, _)  = generate_decls_and_stmts_from_id symbol_table tprogram 
                   tdecl_id tdecl tdecl_type in cdecls @ cdecl
        ) [] tStruct.members in 

        let generate_void_star_params = 
            let anonList = 
                List.filter (fun p -> match p with
                    AnonFuncDecl(anonDecl) -> true
                  | _ -> false) constructor.constructor_params
            in
            let returned_params = 
                List.map (fun p -> match p with
                    AnonFuncDecl(anonDecl) ->
                        (match anonDecl.anon_decl_name with
                            Identifier(s) -> 
                                let paramName = "cap_anon_" ^ s in 
                                (CPointerType(CType(CPrimitiveType(Cvoid)), 1), CIdentifier(paramName)))) anonList
            in
            returned_params
        in
        let extraParams = generate_void_star_params in 

        {
                creturn_type = CType(CPrimitiveType(Cvoid));
                                                
                cfunc_params = [first_param] @ (List.map
                                                (cFuncParam_from_tFuncParam
                                                symbol_table)
                                                constructor.constructor_params)
                @ [last_param]@extraParams;

                cfunc_body = CCompoundStatement(augmented_decls,
                []);
        
                cfunc_name = cconstructor_name;

        } 


let virtual_table_struct_for_tStruct symbol_table tStruct = 
        let all_methods_for_struct = (List.map(Semant.get_fdecl_for_receiver
        tStruct.struct_name symbol_table)
        (Semant.get_unique_method_names_for_struct symbol_table tStruct)) in 

        let print_all_methods_for_struct mthdlist= 
            List.iter (fun fdecl -> 
                Printf.printf "Method name is %s\n" (Astutil.string_of_func fdecl);
            ) mthdlist
        in
        (*(print_all_methods_for_struct all_methods_for_struct);*)
        let methodMemberSymbols = List.map (cDeclaration_from_tFdecl
        symbol_table) all_methods_for_struct in 
        
        let virt_table_name = virtual_table_name_from_tStruct
        tStruct.struct_name in 

        {
                cstruct_name = virt_table_name;
                cstruct_members = methodMemberSymbols;
                cmethod_to_functions = StringMap.empty;
        }


let cStruct_from_tStruct symbol_table tprogram tStruct = 
        let fieldSymbols = List.map (cSymbol_from_sSymbol
        symbol_table) (List.map (Semant.symbol_from_declaration)
        tStruct.members) in

        let all_methods_for_struct = (List.map (Semant.get_fdecl_for_receiver
                tStruct.struct_name symbol_table)
                (Semant.get_unique_method_names_for_struct symbol_table
                tStruct))
        in

        let virtual_table_name = virtual_table_name_from_tStruct
        tStruct.struct_name in 

        let virtual_table_symbol =
                CVarSymbol("_virtual",
                CPointerType(CType(CStruct(virtual_table_name)), 1)) in 

        let defaultStructMemberSymbols = [virtual_table_symbol] @ fieldSymbols in

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
                                           
                                            let generate_void_star_params = 
                                                let anonList = 
                                                    List.filter (fun p -> match p with
                                                        AnonFuncDecl(anonDecl) -> true
                                                      | _ -> false) method_.params
                                                in
                                                let returned_params = 
                                                    List.map (fun p -> match p with
                                                        AnonFuncDecl(anonDecl) ->
                                                            (match anonDecl.anon_decl_name with
                                                                Identifier(s) -> 
                                                                    let paramName = "cap_anon_" ^ s in 
                                                                    (CPointerType(CType(CPrimitiveType(Cvoid)), 1), CIdentifier(paramName)))) anonList
                                                in
                                                returned_params
                                            in
                                            let extraParams = generate_void_star_params in 

 
                                            let cfunc = {
                                                creturn_type = (cType_from_tType
                                                symbol_table
                                                (Semant.type_from_declaration_specifiers
                                                method_.return_type));
                                                
                                                cfunc_params =
                                                        [initial_void_param] @ (List.map
                                                (cFuncParam_from_tFuncParam
                                                symbol_table) method_.params)@extraParams;

                                                cfunc_body =
                                                        CCompoundStatement([init_cast_decl],
                                                []);
        
                                                cfunc_name = cFunc_from_tMethod 
                                                tStruct.struct_name tfunc_name;

                                         } in (StringMap.add tfunc_name cfunc
                                         sym, cfunc_list @ [cfunc])))
                                            (StringMap.empty, []) tStruct.methods) in
        
        let cFunc_for_constructor = 
                cFunc_from_tConstructor symbol_table tprogram tStruct.constructor tStruct in

        let cFunc_for_destructor = cFunc_from_tDestructor symbol_table tprogram
        tStruct.destructor tStruct in 

         ({
                cstruct_name = cStructName_from_tStruct tStruct.struct_name;
                cstruct_members = cStructMemberSymbols;
                cmethod_to_functions = methods_to_cfunctions;
        }, cfuncs, (tStruct, cFunc_for_constructor, cFunc_for_destructor))

      
let update_cFunc tSymbol_table tprogram cFunc tFunc  =
        let updated_symbol_table = List.fold_left (fun m symbol -> StringMap.add
        (Semant.get_id_from_symbol symbol) symbol m) tSymbol_table ((Semant.symbols_from_func_params
        tFunc.params) @ ([Semant.symbol_from_receiver tFunc.receiver])) in 

        let CCompoundStatement(decls, stmts) = cFunc.cfunc_body in 
        let CCompoundStatement(updated_decls, updated_stmts) = fst (fst (update_statement tFunc.body updated_symbol_table tprogram )) in
        {
                cfunc_name = cFunc.cfunc_name;
                creturn_type = cFunc.creturn_type;
                cfunc_body = CCompoundStatement(decls @ updated_decls,
                updated_stmts);
                cfunc_params = cFunc.cfunc_params; 
        }

let update_cFunc_from_anonDef tSymbol_table tprogram cFunc anonDef =
        let globals = Semant.symbols_from_decls tprogram.globals in
        let builtinDecls = [{
                              return_type = DeclSpecTypeSpec(Int);
                              func_name = DirectDeclarator(Var(Identifier("printf")));
                              params = [FuncParamsDeclared(DeclSpecTypeSpec(String),
                              DirectDeclarator(Var(Identifier("x"))))];
                              receiver = ("", "");
                              body = CompoundStatement([], [])}] in
        let builtinSyms = Semant.symbols_from_fdecls builtinDecls in
        let localDecls = Semant.get_decls_from_compound_stmt anonDef.anon_body in
        let localSyms = Semant.symbols_from_decls localDecls in 
        let paramSyms = Semant.symbols_from_func_params anonDef.anon_params in
        let exceptSyms = Semant.symtable_from_symlist (globals@builtinSyms@paramSyms@localSyms) in 
        let id_exists_in_symtable table id =
            try
                (fun x -> true)(StringMap.find id table)
            with _ ->
                false
        in
        
        let rec fix_expr locals instance_name expr = match expr with
            CBinop(e1, op, e2) ->
                    let fe1 = fix_expr locals instance_name e1 in
                    let fe2 = fix_expr locals instance_name e2 in
                    CBinop(fe1, op, fe2)
           | CAsnExpr(e1, aop, e2) ->
                    let fe1 = fix_expr locals instance_name e1 in
                    let fe2 = fix_expr locals instance_name e2 in
                    CAsnExpr(fe1, aop, fe2)
           | CCastExpr(t, e) -> 
                   let fe = fix_expr locals instance_name e in
                   CCastExpr(t, fe)
           | CPostfix(e, pop) ->
                   let fe = fix_expr locals instance_name e in
                   CPostfix(fe, pop)
           | CCall(i, e1, e2, elist) ->
                   let fe1 = fix_expr locals instance_name e1 in
                   let fe2 = fix_expr locals instance_name e2 in
                   let felist = fix_expr_list locals instance_name elist in
                   CCall(i, fe1, fe2, felist)
           | CDeref(e) -> 
                   let fe = fix_expr locals instance_name e in
                   CDeref(fe)
           | CCompareExpr(e1, lop, e2) ->
                   let fe1 = fix_expr locals instance_name e1 in
                   let fe2 = fix_expr locals instance_name e2 in
                   CCompareExpr(fe1, lop, fe2)
           | CPointify(e) ->
                   let fe = fix_expr locals instance_name e in
                   CPointify(fe)
           | CMemAccess(i, e, id) ->
                   let fe = fix_expr locals instance_name e in
                   let CId(fid) = fix_expr locals instance_name (CId(id)) in
                   CMemAccess(i, fe, fid)
           | CId(CIdentifier(s)) ->
                   if (id_exists_in_symtable tSymbol_table s) then
                        expr
                   else if (id_exists_in_symtable exceptSyms s) then 
                        expr
                   else
                       CMemAccess(1, CId(CIdentifier(instance_name)), CIdentifier(s))
           | CDeclExpr(CDeclaration(declSpecs, initDecl)) -> 
                   let fInitDecl = fix_init_declarator locals instance_name initDecl in
                   CDeclExpr(CDeclaration(declSpecs, fInitDecl))
           | _ -> expr
        
       and fix_expr_list locals instance_name elist = match elist with
            [] -> []
          | [e] -> [fix_expr locals instance_name e]
          | h::t -> [fix_expr locals instance_name h]@(fix_expr_list locals instance_name t)
   
       and fix_init_declarator locals instance_name initDecl = match initDecl with
          | CInitDeclaratorAsn(dd, aop, e) ->
                  let CInitDeclarator(fdd) = fix_init_declarator locals instance_name (CInitDeclarator(dd)) in
                  let fe = fix_expr locals instance_name e in
                  CInitDeclaratorAsn(fdd, aop, fe)
          | _ -> initDecl 

       and fix_declaration locals instance_name decl = match decl with
            CDeclaration(declSpecs, initDecl) ->
                let fidecl = fix_init_declarator locals instance_name initDecl in
                CDeclaration(declSpecs, fidecl)

       and fix_declaration_list locals instance_name declList = match declList with
            [] -> []
          | [d] -> [fix_declaration locals instance_name d]
          | h::t -> [fix_declaration locals instance_name h]@(fix_declaration_list locals instance_name t)

       and fix_statement locals instance_name stmt = match stmt with 
            CExpr(e) -> 
                let fe = fix_expr locals instance_name e in 
                CExpr(fe)
          | CReturn(e) -> 
                let fe = fix_expr locals instance_name e in 
                CReturn(fe)
          | CCompoundStatement(declList, stmtList) ->
                let fdl = fix_declaration_list locals instance_name declList in 
                let fsl = fix_statement_list locals instance_name stmtList in
                CCompoundStatement(fdl, fsl)
          | CIf(e, s1, s2) -> 
                let fe = fix_expr locals instance_name e in 
                let fs1 = fix_statement locals instance_name s1 in
                let fs2 = fix_statement locals instance_name s2 in
                CIf(fe, fs1, fs2)
          | CFor(e1, e2, e3, s) ->
                let fe1 = fix_expr locals instance_name e1 in 
                let fe2 = fix_expr locals instance_name e2 in 
                let fe3 = fix_expr locals instance_name e3 in 
                let fs = fix_statement locals instance_name s in
                CFor(fe1, fe2, fe3, fs)
          | CWhile(e, s) ->
                let fe = fix_expr locals instance_name e in 
                let fs = fix_statement locals instance_name s in
                CWhile(fe, fs)
          | _ -> stmt

       and fix_statement_list locals instance_name stmtList = match stmtList with
            [] -> []
          | [s] -> [fix_statement locals instance_name s] 
          | h::t -> [fix_statement locals instance_name h]@(fix_statement_list locals instance_name t)
                  
        in
        let updated_symbol_list = 
            ((Semant.symbols_from_func_params anonDef.anon_params) @ (Semant.symbols_from_outside_scope_for_anon_def tprogram anonDef) )
        in
        let updated_symbol_table = 
            List.fold_left (fun m symbol -> 
                StringMap.add (Semant.get_id_from_symbol symbol) symbol m)
                    tSymbol_table updated_symbol_list in 
        let anon_name = Semant.find_name_for_anon_def tprogram anonDef in
        let instanceName = "s" ^ anon_name in 
        let structName = "S" ^ anon_name in
        let newDecls = [CDeclaration(CDeclSpecTypeSpecAny(CPointerType(CType(CStruct(structName)), 1)), 
        CInitDeclaratorAsn(CDirectDeclarator(CVar(CIdentifier(instanceName))), Asn, CCastExpr(CPointerType(CType(CStruct(structName)), 1), CId(CIdentifier("capture_struct")))))]
        in
    
       
        let CompoundStatement(decls, _) = anonDef.anon_body in
        let locals = Semant.symbols_from_decls decls in
        let CCompoundStatement(decls, stmts) = cFunc.cfunc_body in 
        let cmpstmt = fst (fst (update_statement anonDef.anon_body updated_symbol_table tprogram )) in
        let CCompoundStatement(updated_decls, updated_stmts) = fix_statement locals instanceName cmpstmt in 
        {
                cfunc_name = cFunc.cfunc_name;
                creturn_type = cFunc.creturn_type;
                cfunc_body = CCompoundStatement(decls @ updated_decls @ newDecls,
                updated_stmts);
                cfunc_params = cFunc.cfunc_params; 
        }

let update_cDestructor tSymbol_table tprogram cFunc tStruct = 
        let updated_symbol_table = List.fold_left (fun m symbol -> StringMap.add
        (Semant.get_id_from_symbol symbol) symbol m) tSymbol_table ((Semant.symbols_from_decls
        (Semant.get_decls_from_compound_stmt
        tStruct.constructor.constructor_body)) @ (Semant.symbols_from_decls
        tStruct.members) @ (Semant.symbols_from_func_params
        tStruct.constructor.constructor_params))  in 

        let ctype = cType_from_tType tSymbol_table
        (CustomType(tStruct.struct_name)) in

        let virtual_table_name = virtual_table_name_from_tStruct
        tStruct.struct_name in

        let ancestor_destructor = Semant.get_ancestors_destructor tSymbol_table
        tStruct in 

        let c_ancestor_destructor_name = destructor_name_from_tStruct
        ancestor_destructor.destructor_name in         
        
        let virtual_table_type =
                CType(CStruct(virtual_table_name)) in

        let parent_destructor_call = 
        if (ancestor_destructor.destructor_name <> tStruct.struct_name &&
        ancestor_destructor.destructor_name <> "") then 
              generate_stmts_for_parent_destructor tSymbol_table tprogram 
              tStruct.destructor tStruct else [] in  

        let head_assignments = List.fold_left (fun assignments tdecl ->
                   let tdecl_id = (Semant.var_name_from_declaration tdecl) in
                   
                   let asn_expr = CExpr(CAsnExpr(CId(CIdentifier(tdecl_id)),
                   Asn, CMemAccess(1,
                   CDeref(CId(CIdentifier("_this"))), CIdentifier(tdecl_id)))) in assignments @ [asn_expr] ) [] tStruct.members in

        let CCompoundStatement(original_decls, stmts) = cFunc.cfunc_body in 
        
        let CCompoundStatement(updated_decls, updated_stmts) = fst (
                        fst (update_statement tStruct.destructor.destructor_body
                        updated_symbol_table tprogram)) in

        let free_this = CIf(CId(CIdentifier("_needs_free")),
        CCompoundStatement([], [CExpr(CFree(CMemAccess(1,
        CDeref(CId(CIdentifier("_this"))),
        CIdentifier("_virtual"))))]@[CExpr(CFree(CDeref(CId(CIdentifier("_this")))))]), CEmptyElse) in

        {
                cfunc_name = cFunc.cfunc_name;
                creturn_type = cFunc.creturn_type;
                cfunc_body = CCompoundStatement(original_decls @ updated_decls,
                head_assignments @ parent_destructor_call @ updated_stmts @ [free_this]);
                cfunc_params = cFunc.cfunc_params;
        }

        
let update_cConstructor tSymbol_table tprogram cFunc tStruct = 
        let updated_symbol_table = List.fold_left (fun m symbol -> StringMap.add
        (Semant.get_id_from_symbol symbol) symbol m) tSymbol_table ((Semant.symbols_from_decls
        (Semant.get_decls_from_compound_stmt
        tStruct.constructor.constructor_body)) @ (Semant.symbols_from_decls
        tStruct.members) @ (Semant.symbols_from_func_params
        tStruct.constructor.constructor_params))  in 

        let ctype = cType_from_tType tSymbol_table
        (CustomType(tStruct.struct_name)) in

        let virtual_table_name = virtual_table_name_from_tStruct
        tStruct.struct_name in


        let virtual_table_type =
                CType(CStruct(virtual_table_name)) in 
        
        let alloc_virtual_table =
                CExpr(CAsnExpr(CMemAccess(1, CDeref(CId(CIdentifier("_this"))),
                CIdentifier("_virtual")),
        Asn, CAlloc(virtual_table_type, CId(CIdentifier(sizeof_string tSymbol_table
        virtual_table_type))))) in 

        let alloc_this = CIf(CId(CIdentifier("_needs_malloc")), CExpr(CAsnExpr(CDeref(CId(CIdentifier("_this"))),
                   Asn, CAlloc(ctype, CId(CIdentifier((sizeof_string tSymbol_table
                   ctype)))))), CExpr(CNoexpr)) in

        let virt_table_assignments = generate_virtual_table_assignments 1 tStruct tSymbol_table
        "(*_this)->_virtual" in 
 
        let (interface_decls, interface_assignments) =
                interface_decl_and_assignments_for_struct (-1) tStruct
                tSymbol_table "_this" in 

        let tail_assignments = List.fold_left (fun assignments tdecl ->
                   let tdecl_id = (Semant.var_name_from_declaration tdecl) in
                   
                   let asn_expr = CExpr(CAsnExpr(CMemAccess(1,
                   CDeref(CId(CIdentifier("_this"))), CIdentifier(tdecl_id)),
                   Asn, CId(CIdentifier(tdecl_id)))) in assignments @ [asn_expr] ) [] tStruct.members in

        let stmts_for_super = if (Semant.constructor_has_super
        tStruct.constructor) then (let
        Expr(Super(expr_list)) = Semant.get_super_expr tStruct.constructor.constructor_body
        in

        generate_stmts_for_super expr_list updated_symbol_table tprogram tStruct.constructor
        tStruct) else [] in

        let CCompoundStatement(original_decls, stmts) = cFunc.cfunc_body in 
        
        let CCompoundStatement(updated_decls, updated_stmts) = fst (
                        fst (update_statement tStruct.constructor.constructor_body
                        updated_symbol_table tprogram)) in
        {
                cfunc_name = cFunc.cfunc_name;
                creturn_type = cFunc.creturn_type;
                cfunc_body = CCompoundStatement(original_decls @ interface_decls
                @ updated_decls,
                [alloc_this] @ [alloc_virtual_table] @ stmts_for_super @ updated_stmts @ stmts @
                tail_assignments@virt_table_assignments @ interface_assignments);
                cfunc_params = cFunc.cfunc_params;
        }

let rec cFunc_from_anonDef symbol_table tprogram anonDef =
    let rec convert_anon_params symbol_table params =
        (match params with
            [] -> [(CPointerType(CType(CPrimitiveType(Cvoid)), 1), CIdentifier("capture_struct"))]
          | [p] -> [cFuncParam_from_tFuncParam symbol_table p]@[(CPointerType(CType(CPrimitiveType(Cvoid)), 1), CIdentifier("capture_struct"))]
          | h::t -> let htype = (cFuncParam_from_tFuncParam symbol_table h) in
                    let ttype = (convert_anon_params symbol_table t) in
                    [htype]@ttype)
    in
    {
    cfunc_name = anonDef.anon_name;
    cfunc_body = CCompoundStatement([], []);    
    cfunc_params = (convert_anon_params symbol_table anonDef.anon_params);
    creturn_type = cType_from_tType symbol_table anonDef.anon_return_type }

and cFunc_list_from_anonDef_list symbol_table tprogram adlist = match adlist with
    [] -> []
  | [x] -> [cFunc_from_anonDef symbol_table tprogram x]
  | h::t -> let hfuncs = [(cFunc_from_anonDef symbol_table tprogram h)] in
            let tfuncs = (cFunc_list_from_anonDef_list symbol_table tprogram t) in
            hfuncs@tfuncs

        
let cProgram_from_tProgram program =
        let updated_program = Semant.update_structs_in_program program in

        let tSymbol_table = Semant.build_symbol_table updated_program in
        let cstructs_and_functions = List.map (cStruct_from_tStruct tSymbol_table program ) updated_program.structs in 

        let cstructs = List.map (fun (structs, _, _) -> structs) cstructs_and_functions in

        let virt_table_structs = List.map (
                virtual_table_struct_for_tStruct tSymbol_table)
        updated_program.structs in  

        let cfuncs_methods = List.concat (List.map (fun(_, methods, _) ->
                methods) cstructs_and_functions) in 

        let cconstructors = List.map (fun(_, _, constructor) -> constructor)
        (List.filter (fun(_, _, (_, const, _)) -> if (const.cfunc_name = "") then false
        else true) cstructs_and_functions) in

        let cdestructors = List.map (fun(_, _, constructor) -> constructor)
        (List.filter (fun(_, _, (_, _, destr)) -> if (destr.cfunc_name = "") then false
        else true) cstructs_and_functions) in


        let cStructs = virt_table_structs @ (List.map (cStruct_from_tInterface
        tSymbol_table) program.interfaces) @ cstructs in

        let tAnonDefs = Semant.anon_defs_from_tprogram program in 
        let cFuncsTranslatedFromAnonDefs = cFunc_list_from_anonDef_list tSymbol_table program tAnonDefs in
        let capture_structs = capture_struct_list_from_anon_def_list program tAnonDefs in 
        let updated_cstructs = cstructs@capture_structs in 
        (* The function bodies have not been filled out yet. Just the parameters
         * and return types *)
        let cDeclaredMethodsAndFuncs = cfuncs_methods @ (List.rev (List.map (cFunc_from_tFunc tSymbol_table)
        (List.filter (fun fdecl ->
                if (fdecl.receiver = ("", "")) then true else
                false) program.functions))) in 
                
        let cUpdatedDeclaredMethodsAndFuncs = List.fold_left (fun acc cFunc ->
                let sym_ = StringMap.find
        cFunc.cfunc_name tSymbol_table in (match sym_ with 
                                | FuncSymbol(_, fdecl) -> acc @ [update_cFunc tSymbol_table program
                                        cFunc fdecl] 
                                | _ -> raise(Failure("error")))
                       ) [] cDeclaredMethodsAndFuncs in

        let cConstructors = List.fold_left (fun acc (tStruct, cConst, _) ->
                acc @ [update_cConstructor tSymbol_table program cConst tStruct]) []
                cconstructors in

        let cDestructors = List.fold_left (fun acc (tStruct, _, cDestr) ->
                acc @ [update_cDestructor tSymbol_table program cDestr tStruct]) []
                cconstructors in


        let anon_def_for_function fn = 
            List.find (fun af ->
                if (af.anon_name = fn.cfunc_name) then
                    true
                else
                    false) tAnonDefs
        in

        let cUpdatedFuncsTranslatedFromAnonDefs =
            List.map (fun f -> 
                let anonDef = anon_def_for_function f in
                update_cFunc_from_anonDef tSymbol_table program f anonDef) cFuncsTranslatedFromAnonDefs
        in
        let cFuncs = cConstructors @ cDestructors @ cUpdatedDeclaredMethodsAndFuncs @ cUpdatedFuncsTranslatedFromAnonDefs
        in
        {
                cstructs = cStructs@capture_structs;
                cglobals = [];
                cfunctions = cFuncs;
        }
