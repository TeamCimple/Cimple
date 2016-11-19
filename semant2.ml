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

let rec type_from_declaration_specifiers = function
   DeclSpecTypeSpec(tspec) -> PrimitiveType(tspec)
 | DeclSpecTypeSpecAny(t) -> t
 | DeclSpecTypeSpecInitList(t, tDeclSpecs) -> CompoundType(t, type_from_declaration_specifiers tDeclSpecs)
 
let symbol_from_declaration = function 
     Declaration(declspec, InitDeclarator(ddecl)) -> VarSymbol(var_name_from_direct_declarator ddecl,
                                                               type_from_declaration_specifiers declspec)
   | _ -> raise(Failure("symbol_from_declaration: Unrecognized declaration"))    

let get_decls_from_compound_stmt stmt = match stmt with 
        CompoundStatement(x, y) -> x
   | _ -> []

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

       (* Build map of global declarations *)
             let check_function func = 
                let local_decls = List.map var_name_from_declaration
                (get_decls_from_compound_stmt func.body) in

                report_duplicate (fun a -> "duplicate local variable: " ^ a)
                (local_decls);

                let symbols = List.fold_left (fun m decl -> if StringMap.mem
                (var_name_from_declaration decl) m then
                        raise(Failure("redefining global")) else StringMap.add
       (var_name_from_declaration decl) decl m) StringMap.empty
       (program.globals @ (get_decls_from_compound_stmt func.body))
       in
 

                if StringMap.is_empty symbols then () else
                        raise(Failure("sdfsdf"))

        in List.iter check_function program.functions;

