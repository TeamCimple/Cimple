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

(* Will need the  for this function to lookup details for the Cstruct *)
let gen_non_pointer_type  t = match t with
    CPrimitiveType(x) -> gen_cprimitive_type x
  | CStruct(s) -> "struct " ^ s

let rec gen_cpointer  ptr = match ptr with
    CPointer(npt) -> gen_non_pointer_type  npt
  | CPointerPointer(pptr) -> "*" ^ (gen_cpointer  pptr)


let rec gen_n_pointers n =
    let rec create_dummy_list_with_size n = match n with
        1 -> [()]
      | 0 -> []
      | _ -> let lst = create_dummy_list_with_size (n - 1) in
             [()]@lst
    in
    let lst = create_dummy_list_with_size n in
    List.fold_left (fun s x -> "*" ^ s) "" lst

let rec gen_ctype  t = 
    match t with
        CType(npt) -> gen_non_pointer_type  npt 
      | CPointerType(ct, n) -> (gen_ctype  ct) ^ (gen_n_pointers n) 
      | CFuncPointer(fs) -> let rtype = gen_ctype  fs.func_return_type in
                            let paramTypes = gen_ctype_list  fs.func_param_types in 
                            rtype ^ "(*)(" ^ paramTypes ^ ")"

and gen_ctype_list  tlist = match tlist with
      [] -> ""
    | [x] -> gen_ctype  x
    | h::t -> (gen_ctype  h) ^ ", " ^ (gen_ctype_list  t)

let gen_id id = match id with 
   | Identifier(id) -> id


let rec gen_cexpr  expr = match expr with
     CBinop(e1, op, e2) -> (gen_cexpr  e1) ^ (gen_op op) ^ (gen_cexpr  e2)
   | CAsnExpr(e1, aop, e2) ->  (gen_cexpr  e1) ^ (gen_assn_op aop) ^ (gen_cexpr  e2)
   | CLiteral(x) -> string_of_int x
   | CFloatLiteral(x) -> string_of_float x
   | CArrayAccess(e1, e2) -> (gen_cexpr e1) ^ "[" ^ (gen_cexpr e2) ^ "]"
   | CStringLiteral(s) -> s
   | CCastExpr(ct, e) -> "(" ^ (gen_ctype  ct) ^ ")" ^ "(" ^ (gen_cexpr  e) ^ ")"
   | CPostfix(e, pfop) -> (gen_cexpr  e) ^ (gen_postfix_op pfop)
   | CCall(n, s, e, elist) ->
           if (n > 0) then 
               let asterisks = gen_n_pointers (n - 1) in
               (gen_cexpr s) ^ "->"^ asterisks ^ (gen_cexpr e) ^ "(" ^ gen_expr_list elist ^ ")"
           else (if (s <> CNoexpr) then
               (gen_cexpr s) ^ "." ^ (gen_cexpr e) ^ "(" ^ gen_expr_list elist ^ ")" else (gen_cexpr e) ^ "(" ^ gen_expr_list elist ^ ")")
   | CAlloc(ct, s) -> (match s with 
                        | CBinop(e1, Mul, e2) -> "malloc(" ^ (gen_cexpr e1) ^
                                                (gen_op Mul) ^ "sizeof(" ^
                                                (gen_cexpr e2) ^ ")" ^ ")"
                        | CId(CIdentifier(s)) -> "malloc(" ^ "sizeof(" ^ s ^ ")"
                        ^ ")" )
   | CCompareExpr(e1, op, e2) -> gen_cexpr e1 ^ gen_logical_op op ^
   gen_cexpr e2
   | CPointify(e) -> "&(" ^ (gen_cexpr  e) ^ ")"
   | CMemAccess(d, e, CIdentifier(a)) -> if (d >= 1) then (gen_cexpr e ^ "->"
   ^a) else (gen_cexpr e
   ^ "." ^ a)
   | CId(CIdentifier(s)) -> s
   | CDeref(e) -> "(*" ^ gen_cexpr e ^ ")"
   | CDeclExpr(d) -> gen_cdeclaration  d
   | _ -> ""

and gen_expr_list expr_list = match expr_list with 
   | [] -> ""
   | [h] -> gen_cexpr h
   | h::t -> gen_cexpr h ^ "," ^ gen_expr_list t
  
and gen_cdirect_declarator  dd = match dd with
     CVar(CIdentifier(s)) -> s

and gen_cdeclarator  d = match d with 
     CDirectDeclarator(dd) -> gen_cdirect_declarator  dd 

and gen_cinit_declarator  initDecl = match initDecl with 
     CInitDeclarator(d) -> gen_cdeclarator  d
   | CInitDeclaratorAsn(d, aop, e) -> (gen_cdeclarator  d) ^ " " ^ (gen_assn_op aop) ^ " " ^ (gen_cexpr  e)

and gen_cdeclaration_specifiers  declSpecs = match declSpecs with
    CDeclSpecTypeSpecAny(ct) -> gen_ctype  ct

and gen_cfunc_param  fparam = match fparam with 
    (ct, CIdentifier(s)) ->
        (match ct with
            CFuncPointer(fsig) -> (* gen_type won't work in this context, 
                                     since we need to insert the parameter name
                                     in the middle of the string *)
                let rtype = (gen_ctype fsig.func_return_type) in 
                let ptypes = (gen_ctype_list fsig.func_param_types) in
                (rtype ^ "(*" ^ s ^ ")(" ^ ptypes ^ ")")
          | _ -> (gen_ctype  ct) ^ " " ^ s)

and gen_cfunc_param_type fparam = match fparam with
    (ct, _) -> (gen_ctype ct)

and gen_cfunc_param_list  plist = match plist with
      [] -> ""
    | [p] -> gen_cfunc_param  p
    | h::t -> (gen_cfunc_param  h) ^ ", " ^ (gen_cfunc_param_list  t)

and gen_cfunc_param_type_list plist = match plist with 
      [] -> ""
    | [p] -> gen_cfunc_param_type p
    | h::t -> (gen_cfunc_param_type h) ^ ", " ^ (gen_cfunc_param_type_list t)

and gen_cdeclaration  dcltn = match dcltn with
      CDeclaration(declSpecs, initDecl) -> (gen_cdeclaration_specifiers  declSpecs) ^ " " ^ (gen_cinit_declarator  initDecl)
  
and gen_cdeclaration_list  dlist = match dlist with
     [] -> ""
   | [d] -> gen_cdeclaration  d ^ ";\n"
   | h::t -> (gen_cdeclaration  h) ^ ";\n" ^ (gen_cdeclaration_list  t)

and gen_cstatement  stmt = match stmt with
     CExpr(e) -> gen_cexpr  e ^ ";\n"
   | CReturn(e) -> "return " ^ (gen_cexpr  e) ^ "; "
   | CCompoundStatement(declList, stmtList) -> "{\n" ^ (gen_cdeclaration_list
   declList) ^ "\n" ^ (gen_cstatement_list  stmtList) ^ "\n}\n"
   | CIf(e, s1, s2) -> "if(" ^ (gen_cexpr  e) ^ "){\n" ^ (gen_cstatement  s1) ^
   (gen_cstatement  s2) ^ "\n}\n"
   | CFor(e1, e2, e3, s) -> let se1 = (gen_cexpr  e1) in 
                            let se2 = (gen_cexpr  e2) in
                            let se3 = (gen_cexpr  e3) in
                            let ss = (gen_cstatement  s) in
                            "for(" ^ se1 ^ "; " ^ se2 ^ "; " ^ se3 ^ ")" ^ ss
   | CWhile(e, s) -> let se = (gen_cexpr  e) in
                     let ss = (gen_cstatement  s) in
                     "while(" ^ se ^ "){\n" ^ ss ^ "}\n"
   | CBreak -> "break;\n" 
  
and gen_cstatement_list  stmtList = match stmtList with
     [] -> ""
   | [s] -> (gen_cstatement  s)
   | h::t -> (gen_cstatement  h) ^ (gen_cstatement_list  t)

and gen_csymbol  s = match s with 
     CVarSymbol(s, t) -> (gen_ctype  t) ^ " " ^ s
   | CFuncSymbol(name, func) -> (gen_cfunc  func)
   | CStructSymbol(name, strct) -> (gen_cstruct  strct) 

and gen_csymbol_list  slist = match slist with
     [] -> ""
   | [s] -> (gen_csymbol  s) ^ ";\n"
   | h::t -> (gen_csymbol  h) ^ ";\n" ^ (gen_csymbol_list  t)

and gen_cstruct  s = 
    let sname = s.cstruct_name in
    let smembers = (gen_csymbol_list  s.cstruct_members) in
    "struct " ^ sname ^ "{\n" ^ smembers ^ "}\n"


and gen_cfunc f =
    let rtype = (gen_ctype  f.creturn_type) in 
    let fname = f.cfunc_name in
    let fparams = (gen_cfunc_param_list  f.cfunc_params) in
    let fbody = (gen_cstatement  f.cfunc_body) in
    rtype ^ " " ^ fname ^ " ( " ^ fparams ^ ")" ^ fbody ^ "\n\n"
 
and gen_cfunc_header f = 
    let rtype = (gen_ctype f.creturn_type) in 
    let fname = f.cfunc_name in 
    let fparams = (gen_cfunc_param_type_list f.cfunc_params) in 
    rtype ^ " " ^ fname ^ " ( " ^ fparams ^ ")" ^ "\n\n" 


let rec print_anon_capture_struct cs = 
    let str = gen_cstruct cs in
    Printf.printf "%s\n\n" str

and print_anon_capture_struct_list cslist = match cslist with
    [] -> ()
  | [x] -> print_anon_capture_struct x
  | h::t -> (print_anon_capture_struct h);
            (print_anon_capture_struct_list t)


let test_anon_defs program  = 
        let updated_program = Semant.update_structs_in_program program in
        let anon_defs = Semant.anon_defs_from_tprogram updated_program in
        let cCaptures = List.rev (Ctree.capture_struct_list_from_anon_def_list program anon_defs) in 
        print_anon_capture_struct_list cCaptures

let gen_cprogram cprogram =
    add_header "stdio" ^ "\n" ^ add_header "stdlib" ^ "\n" ^ (String.concat ";\n" (List.map gen_cdeclaration
    cprogram.cglobals)) ^ (String.concat ";\n" (List.map gen_cstruct
    cprogram.cstructs)) ^ ";\n\n" ^ (String.concat ";\n" (List.map gen_cfunc_header
    cprogram.cfunctions))  ^ ";\n\n" ^ (String.concat "\n" (List.map gen_cfunc
    (cprogram.cfunctions)));

