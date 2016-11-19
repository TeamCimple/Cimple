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
     Declaration(declspec, t) -> (let x =
             Declaration(declspec, t) in
     VarSymbol(var_name_from_declaration x, type_from_declaration_specifiers
     declspec))
   | _ -> raise(Failure("symbol_from_declaration: Unrecognized declaration"))    

let symbols_from_decls decls = List.map symbol_from_declaration decls

let get_id_from_symbol = function
        VarSymbol(id, _) -> id
   | FuncSymbol(id, _, _) -> id

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

        let check_function func = 
                let local_decls = List.map var_name_from_declaration
                (get_decls_from_compound_stmt func.body) in

                report_duplicate (fun a -> "duplicate local variable: " ^ a)
                (local_decls);


                let symbol_table = List.fold_left (fun m symbol -> if StringMap.mem
                (get_id_from_symbol symbol) m then
                        raise(Failure("redefining global")) else StringMap.add
                        (get_id_from_symbol symbol) symbol m) StringMap.empty
                         (symbols_from_decls program.globals @ (symbols_from_decls
                         (get_decls_from_compound_stmt func.body)))
                in
 

                if StringMap.is_empty symbol_table then () else
                        raise(Failure("sdfsdf"))

        in List.iter check_function program.functions;

