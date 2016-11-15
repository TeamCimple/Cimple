open Ast

let rec type_from_declaration_specifiers = function
   DeclSpecTypeSpec(tspec) -> PrimitiveType(tspec)
 | DeclSpecTypeSpecAny(t) -> t
 | DeclSpecTypeSpecInitList(t, tDeclSpecs) -> CompoundType(t, type_from_declaration_specifiers tDeclSpecs)
              

let lookup_type_from_var_name (searchName, (truthVal, foundType)) (varName, varType) = match truthVal with 
   true -> (searchName, (truthVal, foundType))
 | false -> if searchName = varName then (searchName, (true, varType))
            else (searchName, (false, Void))

(* Returns an Ast.tType of an expression. 
 * varLookupList is a list of (<variable_name>, <variable_type>)
 *  tuples for the current scope *)
let rec type_from_expr (varLookupList, expr)= match expr with
   Literal(_) -> Int
 | FloatLiteral (_) -> Float
 | Unop(e, _) -> type_from_expr (varLookupList, e)
 | Binop(e1, _, _) -> type_from_expr (varLookupList, e1) 
 | Postfix(e1, _, _) -> type_from_expr (varLookupList, e1) 
 | Id(id) -> let a = List.fold_left lookup_type_from_var_name (id, (false, Void)) varLookupList in
                        ( match a with 
                          (n, (true, varType)) -> varType
                        | _ -> raise (Failure ("Undeclared identifier")))
 | AsnExpr(id, _, _) -> type_from_expr (varLookupList, Id(id))
 | Noexpr -> Void


