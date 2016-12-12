open Ast open Ctree

let add_header package = "#include <" ^ package ^".h>\n" 

let gen_op op = match op with 
   | Add -> "+"
   | Sub -> "-"
   | Mul -> "*"
   | Div -> "/"
   | And -> "&&"
   | Or -> "||"
   | BitAnd -> "&"
   | BitOr -> "|"
   | Xor -> "^"
   | Not -> "!"
   | Lsh -> "<<"
   | Rsh -> ">>"

let gen_assn_op assn_op = match assn_op with 
   | Asn -> "="
   | MulAsn -> "*="
   | DivAsn -> "/="
   | SubAsn -> "-="
   | AddAsn -> "+="
   | LshAsn -> "=<<"
   | RshAsn -> ">>="

let gen_postfix_op pop = match pop with
   | PostPlusPlus -> "++"
   | PostMinusMinus -> "--"
   | PostEmptyOp -> ""

let gen_logical_op logical_op = match logical_op with 
   | Eql -> "=="
   | NotEql -> "!="
   | Less -> "<"
   | LessEql -> "<="
   | Greater -> ">"
   | GreaterEql -> ">="

let gen_type_qualifier q = match q with
   | Const -> "const"
   | Volatile -> "volatile"

let gen_cprimitive_type pt = match pt with
    Cvoid -> "void" 
  | Cchar -> "char"
  | Cshort -> "short"
  | Cint -> "int"
  | Clong -> "long"
  | Cfloat -> "float"
  | Cdouble -> "double"

(* Will need the csymbol_table for this function to lookup details for the Cstruct *)
let gen_non_pointer_type csymbol_table t = match t with
    CPrimitiveType(x) -> gen_cprimitive_type x
  | CStruct(s) -> "struct " ^ s

let rec gen_cpointer csymbol_table ptr = match ptr with
    CPointer(npt) -> gen_non_pointer_type csymbol_table npt
  | CPointerPointer(pptr) -> "*" ^ (gen_cpointer csymbol_table pptr)

let rec gen_ctype csymbol_table t = 
    let rec create_dummy_list_with_size n = match n with
        1 -> [()]
      | _ -> let lst = create_dummy_list_with_size (n - 1) in
             [()]@lst
    in
    let rec gen_n_pointers n =
        let lst = create_dummy_list_with_size n in
        List.fold_left (fun s x -> "*" ^ s) "" lst
    in
    match t with
        CType(npt) -> gen_non_pointer_type csymbol_table npt 
      | CPointerType(ct, n) -> (gen_n_pointers n) ^ (gen_ctype csymbol_table ct)
      | CFuncPointer(fs) -> let rtype = gen_ctype csymbol_table fs.func_return_type in
                            let paramTypes = gen_ctype_list csymbol_table fs.func_param_types in 
                            rtype ^ "(*)(" ^ paramTypes ^ ")"

and gen_ctype_list csymbol_table tlist = match tlist with
      [] -> ""
    | [x] -> gen_ctype csymbol_table x
    | h::t -> (gen_ctype csymbol_table h) ^ ", " ^ (gen_ctype_list csymbol_table t)

let gen_id id = match id with 
   | Identifier(id) -> id


let rec gen_cexpr csymbol_table expr = match expr with
     CBinop(e1, op, e2) -> (gen_cexpr csymbol_table e1) ^ (gen_op op) ^ (gen_cexpr csymbol_table e2)
   | CAsnExpr(e1, aop, e2) ->  (gen_cexpr csymbol_table e1) ^ (gen_assn_op aop) ^ (gen_cexpr csymbol_table e2)
   | CLiteral(x) -> string_of_int x
   | CFloatLiteral(x) -> string_of_float x
   | CStringLiteral(s) -> s
   | CCastExpr(ct, e) -> "(" ^ (gen_ctype csymbol_table ct) ^ "(" ^ (gen_cexpr csymbol_table e) ^ ")"
   | CPostfix(e, pfop) -> (gen_cexpr csymbol_table e) ^ (gen_postfix_op pfop)
   | CCall(n, s, e, elist) -> raise(Failure("gen_cexpr: TODO: CCall needs gen_cfunc"))
   | CAlloc(ct, n) -> "malloc(" ^ (string_of_int n) ^ ")" 
   | CPointify(e) -> "*(" ^ (gen_cexpr csymbol_table e) ^ ")"
   | CMemAccess(_) -> raise(Failure("gen_cexpr: CMemAccess not yet implemented")) (* Not sure what this translates to *)
   | CId(CIdentifier(s)) -> s
   | CDeclExpr(d) -> gen_cdeclaration csymbol_table d
   | _ -> "" 
  
and gen_cdirect_declarator csymbol_table dd = match dd with
     CVar(CIdentifier(s)) -> s

and gen_cdeclarator csymbol_table d = match d with 
     CDirectDeclarator(dd) -> gen_cdirect_declarator csymbol_table dd 

and gen_cinit_declarator csymbol_table initDecl = match initDecl with 
     CInitDeclarator(d) -> gen_cdeclarator csymbol_table d
   | CInitDeclaratorAsn(d, aop, e) -> (gen_cdeclarator csymbol_table d) ^ " " ^ (gen_assn_op aop) ^ " " ^ (gen_cexpr csymbol_table e)

and gen_cdeclaration_specifiers csymbol_table declSpecs = match declSpecs with
    CDeclSpecTypeSpecAny(ct) -> gen_ctype csymbol_table ct

and gen_cfunc_param csymbol_table fparam = match fparam with 
    (ct, CIdentifier(s)) -> (gen_ctype csymbol_table ct) ^ " " ^ s

and gen_cfunc_param_list csymbol_table plist = match plist with
      [] -> ""
    | [p] -> gen_cfunc_param csymbol_table p
    | h::t -> (gen_cfunc_param csymbol_table h) ^ ", " ^ (gen_cfunc_param_list csymbol_table t)

and gen_cdeclaration csymbol_table dcltn = match dcltn with
      CDeclaration(declSpecs, initDecl) -> (gen_cdeclaration_specifiers csymbol_table declSpecs) ^ " " ^ (gen_cinit_declarator csymbol_table initDecl)
  
and gen_cdeclaration_list csymbol_table dlist = match dlist with
     [] -> ""
   | [d] -> gen_cdeclaration csymbol_table d
   | h::t -> (gen_cdeclaration csymbol_table h) ^ ";\n" ^ (gen_cdeclaration_list csymbol_table t)

and gen_cstatement csymbol_table stmt = match stmt with
     CExpr(e) -> gen_cexpr csymbol_table e ^ ";\n"
   | CReturn(e) -> "return " ^ (gen_cexpr csymbol_table e) ^ "; "
   | CCompoundStatement(declList, stmtList) -> "{\n" ^ (gen_cdeclaration_list csymbol_table declList) ^ "\n" ^ (gen_cstatement_list csymbol_table stmtList) ^ "\n}"
   | CIf(e, s1, s2) -> "if(" ^ (gen_cexpr csymbol_table e) ^ "){\n" ^ (gen_cstatement csymbol_table s1) ^ (gen_cstatement csymbol_table s2) ^ "\n}"
   | CFor(e1, e2, e3, s) -> let se1 = (gen_cexpr csymbol_table e1) in 
                            let se2 = (gen_cexpr csymbol_table e2) in
                            let se3 = (gen_cexpr csymbol_table e3) in
                            let ss = (gen_cstatement csymbol_table s) in
                            "for(" ^ se1 ^ "; " ^ se2 ^ "; " ^ se3 ^ "){\n" ^ ss ^ "\n}"
   | CWhile(e, s) -> let se = (gen_cexpr csymbol_table e) in
                     let ss = (gen_cstatement csymbol_table s) in
                     "while(" ^ se ^ "){\n" ^ ss ^ "}"
   | CBreak -> "break;\n" 
  
and gen_cstatement_list csymbol_table stmtList = match stmtList with
     [] -> ""
   | [s] -> (gen_cstatement csymbol_table s)
   | h::t -> (gen_cstatement csymbol_table h) ^ (gen_cstatement_list csymbol_table t)

and gen_csymbol csymbol_table s = match s with 
     CVarSymbol(s, t) -> (gen_ctype csymbol_table t) ^ " " ^ s
   | CFuncSymbol(name, func) -> (gen_cfunc csymbol_table func)
   | CStructSymbol(name, strct) -> (gen_cstruct csymbol_table strct) 

and gen_csymbol_list csymbol_table slist = match slist with
     [] -> ""
   | [s] -> (gen_csymbol csymbol_table s) ^ ";\n"
   | h::t -> (gen_csymbol csymbol_table h) ^ ";\n" ^ (gen_csymbol_list csymbol_table t)

and gen_cstruct csymbol_table s = 
    let sname = s.struct_name in
    let smembers = (gen_csymbol_list csymbol_table s.struct_members) in
    sname ^ "{\n" ^ smembers ^ "}\n"


and gen_cfunc csymbol_table f =
    let rtype = (gen_ctype csymbol_table f.creturn_type) in 
    let fname = f.cfunc_name in
    let fparams = (gen_cfunc_param_list csymbol_table f.cfunc_params) in
    let fbody = (gen_cstatement csymbol_table f.cfunc_body) in
    rtype ^ " " ^ fname ^ " ( " ^ fparams ^ ") {\n" ^ fbody ^ "\n}\n"  



(*let gen_func fdecl = (gen_decl_specs fdecl.return_type) ^ " " ^ (gen_decl*)
(*fdecl.func_name) ^ "(" ^ (gen_func_params fdecl.params) ^ ")" ^ (gen_statement*)
(*fdecl.body)*)


(*let test_anon_defs program =*)
        (*let anon_defs = Ctree.anon_defs_from_func_decl_list program.functions in*)
        (*let print_list_size l = Printf.printf "Number of anonymous function definitions: %s\n" (string_of_int (List.length l)) in*)
        (*print_list_size anon_defs;*)
        (*Ctree.print_anon_defs anon_defs*)

(*let gen_cprogram program =*)
    (*add_header "stdio" ^ "\n" ^ (String.concat ";\n" (List.map gen_declaration*)
    (*program.globals)) ^ (String.concat ";\n" (List.map gen_struct*)
    (*program.structs)) ^ ";\n\n" ^ (String.concat "\n" (List.map gen_func*)
    (*(List.rev program.functions)));*)

 
(*let gen_program program =*)
    (*add_header "stdio" ^ "\n" ^ (String.concat ";\n" (List.map gen_declaration*)
    (*program.globals)) ^ (String.concat ";\n" (List.map gen_struct*)
    (*program.structs)) ^ ";\n\n" ^ (String.concat "\n" (List.map gen_func*)
    (*(List.rev program.functions)));*)
