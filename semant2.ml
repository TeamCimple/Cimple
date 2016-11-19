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


