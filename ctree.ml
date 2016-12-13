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

let cStructName_from_tStruct name = 
        String.concat "" ["_struct"; name]

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
  | CustomType(s) -> (let sym = StringMap.find s symbol_table in 
                                        match sym with 
                                        | StructSymbol(name, _) ->
                                                        CType(CStruct(cStructName_from_tStruct
                                        name))
                                        | InterfaceSymbol(name, _) ->
                                                        CType(CStruct(cStructName_from_tInterface
                                        name)))
  | AnonFuncType(t, tlist) -> 
          let anonRetType = (cType_from_tType symbol_table t) in 
          let anonParamTypes = List.map (fun x -> (cType_from_tType symbol_table x)) tlist in
          CFuncPointer({ 
              func_return_type = anonRetType;
              func_param_types = anonParamTypes
          })
  | _ -> raise(Failure("Haven't filled out yet"))

let cSymbol_from_sSymbol = function
  | _ -> raise(Failure("Not completed"))
  

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

 (*------------------- Function struct_members_from_anon_body-------------------------
 * This function returns a list of Ast.sSymbols representing the variables referenced within the body of
 * an anonymous function that are declared outside of it's scope. This list will form the data
 * members of a special c struct that will be passed to a normal c function whenevever 
 * an anonymous function in cimple is instantiated.
 *
 * Parameters:
         * symbols: A hash table of symbols from outside the scope of the anonymous function def
         * members: A list of function parameters declared in the anon function definition
         * body: An Ast.tStatement (specifically a CompoundStatement) that is the body of the anonymous function
 *-------------------------------------------------------------------------------- *)

let struct_members_from_anon_body symbols members body = 
 
  let rec members_from_expr symbols members e = match e with 
      Id(id) -> if (id_exists_in_symtable symbols id) == true then
                  [Semant.lookup_symbol_by_id symbols id]
                else if (id_exists_in_symlist members id) == true then
                  []
                else raise(Failure("members_from_expr: Error - undeclared symbol"))
   |  Binop(e1, _, e2) -> let e1Members = members_from_expr symbols members e1 in
                           let e2Members = members_from_expr symbols (members@e1Members) e2 in
                           e1Members@e2Members
   |  AsnExpr(e1, _, e2) -> let e1Members = members_from_expr symbols members e1 in
                            let e2Members = members_from_expr symbols (members@e1Members) e2 in
                            e1Members@e2Members
   |  Postfix(e1, _) -> let e1Members = members_from_expr symbols members e1 in
                            e1Members
   |  Call(_, e, elist) -> let eMembers = members_from_expr symbols members e in 
                           let elistMembers = members_from_expr_list symbols (members@eMembers) elist in
                           eMembers@elistMembers
   | Make(_, elist) -> members_from_expr_list symbols members elist
   | Pointify(e) -> members_from_expr symbols members e
   | MemAccess(e, id2) -> let id1Members = members_from_expr symbols members e in 
                            let id2Members = members_from_expr symbols
                            (members@id1Members) e in
                            id1Members@id2Members
   | AnonFuncDef(def) -> raise(Failure("members_from_expr: Error - nested anonymous functions not supported yet"))
   | DeclExpr(decl) -> members_from_declaration symbols members decl
   | _  -> []

   and members_from_expr_list symbols members elist = match elist with
     [] -> []
   | [x] -> members_from_expr symbols members x
   | h::t -> let hMembers = members_from_expr symbols members h in
             let tMembers = members_from_expr_list symbols members t in
             hMembers@tMembers
   and members_from_init_declarator symbols members initDecl =
    match initDecl with 
       InitDeclaratorAsn(_, _, e) -> members_from_expr symbols members e
     | InitDeclList(l) -> members_from_init_declarator_list symbols members l
      
   and members_from_init_declarator_list symbols members declList =
    match declList with 
      [] -> members_from_expr symbols members Noexpr
    | [x] -> members_from_init_declarator symbols members x
    | h::t -> let hmembers = members_from_init_declarator symbols members h in
              hmembers@(members_from_init_declarator_list symbols (members@hmembers) t)
    
   and members_from_declaration symbols members decl = match decl with
     Declaration(_, initDecl) -> members_from_init_declarator symbols members initDecl 
   | _ -> [] (* Other types of declarations wouldn't reference variables from outside scope *)

   and members_from_declaration_list symbols members declList = match declList with
      [] -> []
   | [x] -> members@(members_from_declaration symbols members x)
   | h::t -> let hmembers = members_from_declaration symbols members h in
             hmembers@(members_from_declaration_list symbols (members@hmembers) t)
    
   and members_from_statement_list symbols members stmtList = match stmtList with
     [] -> members
   | [x] -> members@(members_from_statement symbols members x)
   | h::t -> let hmembers = members_from_statement symbols members h in
             (hmembers)@(members_from_statement_list symbols (members@hmembers) t)

   and members_from_statement symbols members stmt = match stmt with 
     CompoundStatement(decls, stmtList) -> 
       let dmembers = members@(members_from_declaration_list symbols members decls) in
       members_from_statement_list symbols dmembers stmtList
   | Expr(e) -> members_from_expr symbols members e
   | Return(e) -> members_from_expr symbols members e
   | If(e, s1, s2) -> let eMembers = members_from_expr symbols members e in 
                      let s1Members = members_from_statement symbols (members@eMembers) s1 in 
                      let s2Members = members_from_statement symbols (members@eMembers@s1Members) s2 in
                      eMembers@s1Members@s2Members
   | For(e1, e2, e3, s) -> let e1Members = members_from_expr symbols members e1 in
                           let e2Members = members_from_expr symbols (members@e1Members) e2 in
                           let e3Members = members_from_expr symbols (members@e1Members@e2Members) e3 in
                           let sMembers = members_from_statement symbols (members@e1Members@e2Members@e3Members) s in
                           e1Members@e2Members@e3Members
   | While(e, s) -> let eMembers = members_from_expr symbols members e in
                    let sMembers = members_from_statement symbols (members@eMembers) s in
                    eMembers@sMembers 
   | _ -> []
  in

  members_from_statement symbols members body

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
let capture_struct_from_anon_def symbols structName def = 
  let param_symbols = Semant.symbols_from_func_params def.anon_params in 
    {
      struct_name = structName;
      struct_members = List.map cSymbol_from_sSymbol (struct_members_from_anon_body
      symbols param_symbols def.anon_body);
      method_to_functions = StringMap.empty;
    }

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

(*let cFunc_from_anonDef symbol_table anonDef =*)
    (*let rec convert_anon_params symbol_table params =*)
        (*(match params with*)
            (*[] -> [(CPointerType(CType(CPrimitiveType(Cvoid)), 1), CIdentifier("capture_struct"))]*)
          (*| [p] -> [cFuncParam_from_tFuncParam symbol_table p]*)
          (*| h::t -> let htype = (cFuncParam_from_tFuncParam symbol_table h) in*)
                    (*let ttype = (convert_anon_params symbol_table t) in*)
                    (*[htype]@ttype)*)
    (*in {*)
    (*cfunc_name = anonDef.anon_name;*)
    
    (*cfunc_params = (convert_anon_params symbol_table anonDef.anon_params);*)
    (*cfunc_return_type = cType_from_tType symbol_table anonDef.anon_return_type*)
(*}*)

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

let cFunc_from_tMethod cStruct_Name tFuncName = String.concat "_"
[cStruct_Name;tFuncName]

let cStruct_from_tStruct symbol_table tStruct = 
        let defaultStructMemberSymbols = List.map cSymbol_from_sSymbol (List.map (Semant.symbol_from_declaration)
        tStruct.members) in 

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
           
let c_init_decl_from_string str = 
       CInitDeclarator(CDirectDeclarator(CVar(CIdentifier(str))))

       
        

let update_decl decl tSymbol_table cSymbol_table = 
                let id = Semant.var_name_from_declaration decl in 
                   let tType = Semant.type_from_identifier tSymbol_table (Identifier(id))
                   in 
                   
                   match (tType) with 
                   | PrimitiveType(t) -> raise(Failure("not supported"))
                   | CustomType(t) -> (let sym = Semant.lookup_symbol_by_id
                   tSymbol_table (Identifier(id)) in ( match sym with
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
                                                      CDeclaration(CDeclSpecTypeSpecAny(CPointerType(CType(CStruct(cStructName_from_tInterface
                                                interface_.name)), 1)),
                                                c_init_decl_from_string (String.concat ""
                                                ["_";id;(cStructName_from_tInterface
                                                interface_.name)])) in

                                                let cdecl = (match decl with
                                                        | Declaration(_,
                                                        InitDeclarator(DirectDeclarator(_)))
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

                                                let cStruct = StringMap.find
                                                (cStructName_from_tStruct
                                                st.struct_name) cSymbol_table in
                                                
                                                (* Assign the C function
                                                        * associated with the
                                                        * method to the
                                                        * corresponding field of
                                                        * the newly created
                                                        * interface struct *) 
                                                let method_asns = List.map (fun
                                                        tmethod_name
                                                -> CExpr(CAsnExpr(CMemAccess(1,
                                                (CId(CIdentifier(String.concat ""
                                                ["_";id;(cStructName_from_tInterface
                                                interface_.name)]))),
                                                (CIdentifier(tmethod_name))),
                                                Asn,
                                                (let cfunc = StringMap.find
                                                tmethod_name
                                                cStruct.method_to_functions in
                                                (CId(CIdentifier(cfunc.cfunc_name)))))))
                                                (List.map (fun fdecl ->
                                                        Semant.var_name_from_direct_declarator
                                                fdecl.func_name) interface_.funcs) in 

                                                let reference_implementer_asn =
                                                        CExpr(CAsnExpr(CMemAccess(1,
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
                                                        CIdentifier(cStructName_from_tInterface
                                                interface_.name)), Asn, (CId(CIdentifier(String.concat ""
                                                ["_";id;(cStructName_from_tInterface
                                                interface_.name)]))))) in 

                                                let assignments = method_asns @
                                                [reference_implementer_asn] @
                                                [implementer_add_interface_assn]
                                                in 

                                                (decls, assignments) 

                                       ) else ([], [])
                                        | _ -> raise(Failure("Non struct type
                                        decl'd"))))

let cFunction_from_tMethod object_type method_ cSymbol_table tSymbol_table = 
        match object_type  with 
        | CustomType(name) -> ( let typ_symbol  =
                Semant.lookup_symbol_by_id tSymbol_table (Identifier(name)) in match
                typ_symbol with 
                | StructSymbol(_, _) -> cFunc_from_tMethod
                (cStructName_from_tStruct name) method_
                | InterfaceSymbol(_, _) -> method_)
        | PointerType(CustomType(name), 1) -> ( let typ_symbol  =
                Semant.lookup_symbol_by_id tSymbol_table (Identifier(name)) in match
                typ_symbol with 
                | StructSymbol(_, _) -> cFunc_from_tMethod
                (cStructName_from_tStruct name) method_ 
                | InterfaceSymbol(_, _) -> method_)
        | _ -> raise(Failure("not done"))
(*
 * This function takes a cimple expression and returns the pair (C expression,
 * statement[]). The idea is that some cimple expression may generate multiple
 * assignment statements such as a make expression with a constructor.
 * Expressions should not create more declarations, only declarations create
 * more declarations.
 *)
let rec update_expr texpr tSymbol_table cSymbol_table = match texpr with
        | Binop(e1, op, e2) -> (let (updated_e1, e1_stmts) = update_expr e1
                                tSymbol_table cSymbol_table in 
                        let (updated_e2, e2_stmts) = update_expr e2
                        tSymbol_table cSymbol_table in 

                        (CBinop(updated_e1, op, updated_e1), e1_stmts @
                        e2_stmts))

        | AsnExpr(e1, op, e2)  -> (let (updated_e1, e1_stmts) = update_expr e1
                                tSymbol_table cSymbol_table in 
                        let (updated_e2, e2_stmts) = update_expr e2
                        tSymbol_table cSymbol_table in 

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
                                        tSymbol_table cSymbol_table s expr_list                  
     | MemAccess(expr, Identifier(s)) -> (let typ_ = Semant.type_from_expr
     tSymbol_table expr in match (typ_) with 
                | CustomType(name) -> (CMemAccess(0, fst (update_expr expr
                tSymbol_table cSymbol_table), CIdentifier(s)), [])
                | PointerType(CustomType(name), 1) -> (CMemAccess(1, fst
                (update_expr expr tSymbol_table cSymbol_table), CIdentifier(s)),
                [])
                | _ -> raise(Failure("Bad Mem Access")))
     | Id(Identifier(s)) -> (CId(CIdentifier(s)), [])
     | Literal(d) -> (CLiteral(d), [])
     | FloatLiteral(d) -> (CFloatLiteral(d), [])
     | StringLiteral(s) -> (CStringLiteral(s), [])
     | Postfix(e1, op) -> (let (updated_e1, e1_stmts) = update_expr e1
                                tSymbol_table cSymbol_table in 
                        (CPostfix(updated_e1, op), e1_stmts))
     | _ -> raise(Failure("not finished"))
 
and cExpr_from_tExpr_in_tCall tSymbol_table cSymbol_table tExpr tFuncParam = 
      let expr_type = Semant.type_from_expr
        tSymbol_table tExpr in let param_type = Semant.type_from_func_param
        tFuncParam in  match (expr_type, param_type) with
                        |  (CustomType(a), CustomType(b)) -> if (Semant.is_interface
                        tSymbol_table (Identifier(b))) then CMemAccess(0, fst (update_expr tExpr
                        tSymbol_table cSymbol_table),
                        CIdentifier(cStructName_from_tInterface b)) else ( if
                                (Semant.t1_inherits_t2 a b tSymbol_table) then
                                        CCastExpr(CType(CStruct(cStructName_from_tStruct
                                        b)), fst (update_expr tExpr
                                        tSymbol_table cSymbol_table)) else fst (update_expr tExpr
                                        tSymbol_table cSymbol_table))
                        | (PointerType(CustomType(a), 1),
                        CustomType(b)) ->  CMemAccess(1, fst (update_expr tExpr
                        tSymbol_table cSymbol_table),
                        CIdentifier(cStructName_from_tInterface b))
                        | _ -> fst (update_expr tExpr tSymbol_table
                        cSymbol_table)

and cCallExpr_from_tCallExpr expr tSym cSym func_name expr_list = match expr with
        | Noexpr -> let FuncSymbol(_, fdecl) = StringMap.find func_name tSym in (CCall(0,
        CNoexpr, CId(CIdentifier(func_name)), (List.map2
        (cExpr_from_tExpr_in_tCall tSym cSym) expr_list fdecl.params)), [])

        | _ -> let expr_type = Semant.type_from_expr tSym expr in (match expr_type with 
                | CustomType(a) -> let fdecl = Semant.get_fdecl_for_receiver a
                func_name tSym in 

                        if (Semant.is_interface tSym (Identifier(a))) then
                                (CCall(1, (fst (update_expr expr tSym cSym)),
                                CId(CIdentifier(a)), (List.map2
                                (cExpr_from_tExpr_in_tCall tSym cSym) expr_list
                                fdecl.params)), [])
                        else
                                let first_arg =
                                        CCastExpr(CPointerType(CType(CPrimitiveType(Cvoid)),
                                        1), CPointify(fst (update_expr expr tSym
                                        cSym))) in 
                                (CCall(0, CNoexpr,
                                CId(CIdentifier(cFunc_from_tMethod a
                                func_name)), [first_arg] @ (List.map2
                                (cExpr_from_tExpr_in_tCall tSym cSym) expr_list
                                fdecl.params)), [])
               | PointerType(CustomType(a), 1) -> let fdecl =
                       Semant.get_fdecl_for_receiver a func_name tSym in 
                                let first_arg =
                                        CCastExpr(CPointerType(CType(CPrimitiveType(Cvoid)),
                                        1), fst (update_expr expr tSym
                                        cSym)) in 
                                (CCall(0, CNoexpr,
                                CId(CIdentifier(cFunc_from_tMethod a
                                func_name)), [first_arg] @ (List.map2
                                (cExpr_from_tExpr_in_tCall tSym cSym) expr_list
                                fdecl.params)), [])
               | _ -> raise(Failure("No other functions can call methods")))
                
               

(*                                
let rec update_statement tstmt tSymbol_table cSymbol_table =  match tstmt with 
        | CompoundStatement(decls, stmts) -> 
        | EmptyElse -> (CEmptyElse, [])
        | Return(e) -> 
        | If(e, stmt, stmt) -> 
        | For(e1, e2, e3, stmt) ->
        | While(e1, stmt) ->
        | Break ->  
        
*)      

let cFunc_from_anonDef symbol_table anonDef =
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


let cProgram_from_tProgram program =
        let updated_program = Semant.update_structs_in_program program in

        let tSymbol_table = Semant.build_symbol_table updated_program in

        let cstructs_and_functions = List.map (cStruct_from_tStruct
        tSymbol_table) updated_program.structs in 

        let cstructs = List.map fst cstructs_and_functions in 

        let cfuncs_methods = List.concat (List.map snd cstructs_and_functions) in 

        let cStructs = (cstructs @ List.map (cStruct_from_tInterface
        tSymbol_table) program.interfaces) in

        (*let  *)

        (* The function bodies have not been filled out yet. Just the parameters
         * and return types *)
        let cFuncs = cfuncs_methods @ (List.map (cFunc_from_tFunc tSymbol_table)
        program.functions)
        in
 
        {
                cstructs = cStructs;
                cglobals = [];
                cfunctions = cFuncs;
        }
