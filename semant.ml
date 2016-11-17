open Ast

let find_symbol_helper s (truthValue, searchName) = match s with
     VarSymbol(name, _) -> if truthValue == true then (true, searchName)
                           else if name != searchName then (false, searchName)
                           else (true, searchName)
   | FuncSymbol(name, _, _) -> if truthValue == true then (true, searchName)
                               else if name != searchName then (false, searchName)
                               else (true, searchName)

let symbol_exists symbols searchName = 
        let (result, _) = List.fold_right find_symbol_helper symbols (false, searchName) in
        result

let lookup_symbol_from_id symbols id = match id with 
   Id(Identifier(varName)) -> let f sym (truthValue, (name, foundSym)) = (match (truthValue, sym) with
                                                        (true, _) -> (true, (name, foundSym))
                                                      | (false, VarSymbol(_name,_)) -> if (name == _name) then (true, (name, sym))
                                                                                      else (false, (name, foundSym))
                                                      | (false, FuncSymbol (_name, _, _)) -> if (name == _name) then (true, (name, sym))
                                                                                             else (false, (name, foundSym)))
                              in
                              let (truthResult, (_, theSym)) = List.fold_right f symbols (false, (varName, VarSymbol("DummyName", PrimitiveType(Void)))) in
                              (match truthResult with
                                 true -> theSym
                               | false -> raise(Failure("Undeclared identifier")) )
 | _ -> raise(Failure("symbol_from_id: parameter is an expression, but not an identifier"))

let check_compatible_types symbols (t1, t2) = match (t1, t2) with
    (PrimitiveType(pt1), PrimitiveType(pt2)) -> if pt1 == Void || pt2 == Void then
                                                raise(Failure("Cannot assign to void type"))
                                                else ()
                                            (*else (locals@[symbol_from_declaration decl], globals)*)
   | (CustomType(_), CustomType(_)) -> () 
   | (StringType, StringType) -> ()
   | _ -> raise(Failure("check_compatible_types: CompoundType not yet supported"))

let rec type_from_declaration_specifiers = function
   DeclSpecTypeSpec(tspec) -> PrimitiveType(tspec)
 | DeclSpecTypeSpecAny(t) -> t
 | DeclSpecTypeSpecInitList(t, tDeclSpecs) -> CompoundType(t, type_from_declaration_specifiers tDeclSpecs)
              

let lookup_type_from_var_name (searchName, (truthVal, foundType)) (varName, varType) = match truthVal with 
   true -> (searchName, (truthVal, foundType))
 | false -> if searchName = varName then (searchName, (true, varType))
            else (searchName, (false, Void))

let rec type_from_expr symbols expr = match expr with
   Literal(_) -> PrimitiveType(Int)
 | FloatLiteral (_) -> PrimitiveType(Float)
 | Unop(e, _) -> type_from_expr symbols e
 | Binop(e1, _, _) -> type_from_expr symbols e1 
 | Postfix(e1, _, _) -> type_from_expr symbols e1
 | Id(id) -> let sym = lookup_symbol_from_id symbols expr in 
             (match sym with
                VarSymbol(_, t) -> t
              | FuncSymbol(_, t, _) -> t)
 | AsnExpr(id, _, _) -> type_from_expr symbols expr
 | Noexpr -> PrimitiveType(Void)

let type_from_func_param = function
     FuncParamsDeclared(declspec, _) -> type_from_declaration_specifiers declspec
   | ParamDeclWithType(declspec) -> type_from_declaration_specifiers declspec

let var_name_from_direct_declarator = function
     DirectDeclarator(Var(Identifier(s))) -> s
   | _ -> raise(Failure("Pointers not yet supported"))

let var_name_from_declaration = function 
     Declaration(_ , InitDeclarator(dd)) -> var_name_from_direct_declarator dd 
   | Declaration(_, InitDeclaratorAsn(dd, _, _)) -> var_name_from_direct_declarator dd
   | _ -> raise(Failure("var_name_from_declaration: not yet supported"))

let build_param_list acc param = [type_from_func_param param] @ acc

(* Convenience function that returns a tSymbol from a tFuncDecl argument *)
let symbol_from_function fdecl = FuncSymbol(var_name_from_direct_declarator fdecl.func_name,
                                            type_from_declaration_specifiers fdecl.return_type,
                                            List.rev (List.fold_left build_param_list [] fdecl.params))

let symbol_from_declaration = function 
     Declaration(declspec, InitDeclarator(ddecl)) -> VarSymbol(var_name_from_direct_declarator ddecl,
                                                               type_from_declaration_specifiers declspec)
   | _ -> raise(Failure("symbol_from_declaration: Unrecognized declaration"))    

let rec check_expr symbols e = match e with
     Id(Identifier(name)) -> if (symbol_exists symbols name) == false then
                                raise(Failure("Undeclared identifier"))
                             else ()
   | Binop(e1, _, e2) -> let t1 = type_from_expr symbols e1 in
                         let t2 = type_from_expr symbols e2 in 
                         check_compatible_types symbols (t1, t2)
   | Unop(e, unop) -> check_expr symbols e;
                      let te = type_from_expr symbols e in 
                      (match (te, unop) with
                          (PrimitiveType(Void), _) -> raise(Failure("Cannot apply unary operator to void type"))
                        | (PrimitiveType(_), _) -> ()
                        | _ -> raise(Failure("Type/Unary Operator mismatch")))
                        

(* check_local_declaration is meant to be used as an argument to
 *  a List.fold_left to iterate a functions declaration list *)
let rec check_local_declaration decl (locals, globals) = match decl with
     Declaration(declspec, InitDeclarator(ddecl)) -> if symbol_exists locals (var_name_from_declaration decl) == true then 
                                                        raise(Failure("Duplicate identifier")) 
                                                     else (locals@[symbol_from_declaration decl], globals)
   | Declaration(declspec, InitDeclaratorAsn(declarator, asnOp, expr)) -> 
                   (*ignore(check_expr locals@globals expr);*)
                   (*ignore(symbol_exists locals (var_name_from_declaration decl)); [>Check not already declared <]*)
                   let sym = symbol_from_declaration decl in
                   (match sym with
                       VarSymbol(_, t1) -> let t2 = type_from_expr locals expr in
                                               check_compatible_types locals (t1, t2);
                                               (locals@[sym], globals)
                     | FuncSymbol(_, t1, _) -> let t2 = type_from_expr locals expr in 
                                                check_compatible_types locals (t1, t2);
                                               (locals@[sym], globals))
  | Declaration(declspec, InitDeclList(x)) -> let build_declaration_list l d = Declaration(declspec, d)::l in 
                                              let declList = List.rev (List.fold_left build_declaration_list [] x) in
                                              List.fold_right check_local_declaration declList (locals, globals)
                                                          
