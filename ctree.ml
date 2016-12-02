open Ast

module StringMap = Map.Make(String)

type cStruct = {
  struct_name: string;
  struct_members: Ast.sSymbol list; 
}

let id_exists_in_symtable symbols id = 
  try 
    StringMap.find (Astutil.string_of_identifier id) symbols;
    true
  with _ -> false 

let id_exists_in_symlist symlist id = 
  let check_sym_id_equal sym id = 
    match sym with
      VarSymbol(name, _) -> name == (Astutil.string_of_identifier id)
    | FuncSymbol(name, _, _, (_, _)) -> name == (Astutil.string_of_identifier id)
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
    | FuncSymbol(name, _, _, (_, _)) -> name == (Astutil.string_of_identifier id)
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
 *-------------------------------------------------------------------------------- *)

let struct_members_from_anon_body symbols members body = 
  
  let members_from_expr symbols members e = match e with 
      Id(id) -> if (id_exists_in_symtable symbols id) == true then
                  [Semant.lookup_symbol_by_id symbols id]
                else if (id_exists_in_symlist members id) == true then
                  [lookup_symbol_from_symlist_by_id members id]
                else []
    | _  -> [] 
  in
  let rec members_from_init_declarator symbols members initDecl =
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
 * ------------------------------------------------------------------------*)
let capture_struct_from_anon_def symbols structName def = 
  let param_symbols = Semant.symbols_from_func_params def.anon_params in 
    {
      struct_name = structName;
      struct_members = struct_members_from_anon_body symbols param_symbols def.anon_body
    }
  
