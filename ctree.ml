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
        structs: cStruct list;
        globals: cDeclaration list;
        functions: cFunc list;
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
   | CPostfix of cExpr * tPostfixOperator * cExpr
   | CCall of string * cExpr * cExpr list
   | CAlloc of cType * int
   | CPointify of cExpr
   | CMemAccess of cIdentifier * cIdentifier
   | CId of cIdentifier
   | CDeclExpr of cDeclaration
   | CNoExpr

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

let cType_from_tType symbol_table = function
    PrimitiveType(typeSpec) -> cType_from_tTypeSpec typeSpec
  | CustomType(s) -> (let sym = StringMap.find s symbol_table in 
                                        match sym with 
                                        | StructSymbol(name, _) ->
                                                        CType(CStruct(cStructName_from_tStruct
                                        name))
                                        | InterfaceSymbol(name, _) ->
                                                        CType(CStruct(cStructName_from_tInterface
                                        name)))
  | _ -> raise(Failure("Haven't filled out yet"))

let cSymbol_from_sSymbol = function
  | _ -> raise(Failure("Not completed"))

(*let string_of_cStruct s = "CStruct(Name: " ^ s.struct_name ^ ", Symbols: " ^
 * Astutil.string_of_symbol_list s.struct_members ^ ")"*)

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
   |  Postfix(e1, _, e2) -> let e1Members = members_from_expr symbols members e1 in
                            let e2Members = members_from_expr symbols (members@e1Members) e2 in
                            e1Members@e2Members
   |  Call(_, e, elist) -> let eMembers = members_from_expr symbols members e in 
                           let elistMembers = members_from_expr_list symbols (members@eMembers) elist in
                           eMembers@elistMembers
   | Make(_, elist) -> members_from_expr_list symbols members elist
   | Pointify(e) -> members_from_expr symbols members e
   | MemAccess(id1, id2) -> let id1Members = members_from_expr symbols members (Id(id1)) in 
                            let id2Members = members_from_expr symbols (members@id1Members) (Id(id2)) in
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
(* Return (updatedAnonCounter, tFuncDecl) *)
let c_function_from_anon_function anonCounter anonDef = {
        return_type = DeclSpecTypeSpecAny(anonDef.anon_return_type);
        func_name = DirectDeclarator(Var(Identifier("anonFunc_" ^ string_of_int (anonCounter + 1))));
        receiver = ("", "");
        params = anonDef.anon_params;
        body = anonDef.anon_body
}

let rec anon_defs_from_expr = function
     AnonFuncDef(anonDef) -> [anonDef]
   | Binop(e1, op, e2) -> (anon_defs_from_expr e1)@(anon_defs_from_expr e2)
   | AsnExpr(_, _, e) -> anon_defs_from_expr e
   | Postfix(e1, _, e2) -> (anon_defs_from_expr e1)@(anon_defs_from_expr e2)
   | Call(_, e, elist) -> (anon_defs_from_expr e)@(anon_defs_from_expr_list elist)
   | _ -> [] (* Other expression types cannot possibly contain anonymous function definitions *) 

and anon_defs_from_expr_list = function 
     [] -> []
   | [e] -> anon_defs_from_expr e
   | h::t -> (anon_defs_from_expr h)@(anon_defs_from_expr_list t)

let rec anon_defs_from_declaration = function
     Declaration(declSpecs, initDecl) -> anon_defs_from_init_declarator initDecl

and anon_defs_from_declaration_list = function
     [] -> []
   | [d] -> anon_defs_from_declaration d
   | h::t -> (anon_defs_from_declaration h)@(anon_defs_from_declaration_list t)

and anon_defs_from_init_declarator = function
           InitDeclaratorAsn(_, _, e) -> anon_defs_from_expr e
         | InitDeclList(initDeclList) -> anon_defs_from_init_declarator_list initDeclList
         | _ -> []

and anon_defs_from_init_declarator_list = function
     [] -> []
   | [decl] -> anon_defs_from_init_declarator decl
   | h::t -> (anon_defs_from_init_declarator h)@(anon_defs_from_init_declarator_list t)

let rec anon_defs_from_statement stmt = match stmt with
     Expr(e) -> anon_defs_from_expr e
   | Return(e) -> anon_defs_from_expr e
   | If(e, s1, s2) -> (anon_defs_from_expr e)@(anon_defs_from_statement s1)@(anon_defs_from_statement s2)
   | For(e1, e2, e3, s) -> (anon_defs_from_expr e1)@(anon_defs_from_expr e2)@(anon_defs_from_expr e3)@(anon_defs_from_statement s)
   | While(e, s) -> (anon_defs_from_expr e)@(anon_defs_from_statement s)
   | CompoundStatement(declList, stmtList) -> (anon_defs_from_declaration_list declList)@(anon_defs_from_statement_list stmtList)

and anon_defs_from_statement_list = function
     [] -> []
   | [s] -> anon_defs_from_statement s
   | h::t -> anon_defs_from_statement_list t

<<<<<<< HEAD
(* Return tAnonFuncDef list *)
(*let collect_anon_defs_for_func_decl fdecl = *)


let cDeclarationsAndStatements_from_tDeclaration = function
        | Declaration(decl_specs, InitDeclList

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

let cStruct_from_tStruct symbol_table tStruct = 
        let defaultStructMemberSymbols = List.map cSymbol_from_sSymbol (List.map (Semant.symbol_from_declaration)
        tStruct.members) in 

        (* If there is an interface then add a struct member corresponding to
         * the interface to our struct *)
        let cStructMemberSymbols = if (tStruct.implements <> "") then
                [cSymbol_from_Implements tStruct.implements] @
                defaultStructMemberSymbols else defaultStructMemberSymbols in

        
        let (methods_to_cfunctions, cfuncs) = (List.fold_left (fun (sym, cfunc_list) method_ -> 
                                            (let tfunc_name =
                                                    Semant.var_name_from_direct_declarator
                                               method_.func_name in
                                                       
                                            let cfunc = {
                                                creturn_type = (cType_from_tType
                                                symbol_table
                                                (Semant.type_from_declaration_specifiers
                                                method_.return_type));
                                                
                                                cfunc_params = (List.map
                                                (cFuncParam_from_tFuncParam
                                                symbol_table) method_.params);

                                                cfunc_body = CCompoundStatement([],
                                                []);
        
                                                cfunc_name = String.concat "_"
                                                [(cStructName_from_tStruct
                                                tStruct.struct_name);
                                                tfunc_name];
                                         } in (StringMap.add tfunc_name cfunc
                                         sym, cfunc_list @ [cfunc])))
                                            (StringMap.empty, []) tStruct.methods) in

        ({
                struct_name = cStructName_from_tStruct tStruct.struct_name;
                struct_members = cStructMemberSymbols;
                method_to_functions = methods_to_cfunctions;
        }, cfuncs)
                        
(*let cFunc_from_tFunc uid tFunc symbol_table =
        let tfunc_name = Semant.var_name_from_direct_declarator tFunc.func_name
        in
        let cfunc_name = if (tFunc.receiver = ("", "")) then func_name else
                        String.concat (String.concat "_" (fst tFunc.receiver))
                        func_name in
        let cFunc = {
                func_name = cfunc_name;
        }*)


let cProgram_from_tProgram program =
        let updated_program = Semant.update_structs_in_program program in

        let tSymbol_table = Semant.build_symbol_table updated_program in

        let cstructs_and_functions = List.map (cStruct_from_tStruct
        tSymbol_table) updated_program.structs in 

        let cstructs = List.map fst cstructs_and_functions in 

        let cfuncs_methods = List.map snd cstructs_and_functions in 

        let cStructs = (cstructs @ List.map (cStruct_from_tInterface
        tSymbol_table) program.interfaces) in

        (* The function bodies have not been filled out yet. Just the parameters
         * and return types *)
        let cFuncs = cfuncs_methods @ (List.map (cFunc_from_tFunc tSymbol_table)
        program.functions)
        in

        let cGlobals = 

        let cprogram =  
        {
                structs = cStructs;
                globals = [];
                functions = [];
        }
=======

let rec anon_defs_from_func_decl = function
    _ -> []

and anon_defs_from_func_decl_list = function
      [] -> []
    | [x] -> anon_defs_from_func_decl x
    | h::t -> (anon_defs_from_func_decl h)@(anon_defs_from_func_decl_list t)
>>>>>>> a3030a87c3baa06e0f70b8fd847c5af82525576c
