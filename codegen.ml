open Ast

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
   | PostDeref -> "."
   | PostEmptyOp -> ""

let gen_logical_op logical_op = match logical_op with 
   | Eql -> "=="
   | NotEql -> "!="
   | Less -> "<"
   | LessEql -> "<="
   | Greater -> ">"
   | GreaterEql -> ">="

let gen_address_op address_op = match address_op with
   | Ampersand -> "&"

let gen_type_qualifier q = match q with
   | Const -> "const"
   | Volatile -> "volatile"

let gen_type_spec ts = match ts with 
   | Void -> "void"
   | Char -> "char"
   | Short -> "short"
   | Int -> "int"
   | Long -> "long"
   | Float -> "float"
   | Double -> "double"
   | Bool -> "bool"
   | Unsigned -> "unsigned"
   | String -> "const char*" (*Probably should do this smarter*)

let gen_id id = match id with 
   | Identifier(id) -> id

let gen_pointer ptr = match ptr with
   | PtrType(p1, p2) -> "**" (* Not sure if ** is the only expected putput for PtrType*)
   | Pointer -> "*"

let rec gen_expr expr = match expr with 
   | Binop(e1, op, e2) -> (gen_expr e1) ^ (gen_op op) ^ (gen_expr e2)
   | AsnExpr(Identifier(id), assn_op, e2) -> id ^ (gen_assn_op assn_op) ^
   (gen_expr e2)
   | Literal(x) -> string_of_int x
   | FloatLiteral(x) -> string_of_float x
   | StringLiteral(x) -> x
   | Id(Identifier(id)) -> id
   | Call(_, Id(Identifier(id)), elist) -> id ^ "(" ^ (String.concat ","  (List.map
   gen_expr elist)) ^ ")"
   | MemAccess(Identifier(s), Identifier(t)) -> s ^ "." ^ t
   | Postfix(e1, pop, e2) -> gen_expr e1 ^ (gen_postfix_op pop) ^ (gen_expr e2)
   | Address(address_op, e) -> (gen_address_op address_op) ^ (gen_expr e)
   | Noexpr -> ""

let gen_direct_decl d = match d with 
   | Var(Identifier(id)) -> id
   | _ -> raise(Failure("do not have arrays implemented"))

let gen_decl d = match d with
   | DirectDeclarator(x) -> gen_direct_decl x
   | PointerDirDecl(p, x) -> (gen_pointer p) ^ (gen_direct_decl x)
   
let rec gen_init_decl idecl = match idecl with
   | InitDeclarator(decl) -> gen_decl decl
   | InitDeclList(decl_list) -> ( match decl_list with 
                [] -> ""
                | [InitDeclarator(decl)] -> gen_decl decl
                | [InitDeclaratorAsn(decl, asn_op, expr)] -> gen_decl decl ^
                (gen_assn_op asn_op) ^ (gen_expr expr)
                | h :: t -> gen_init_decl h ^ "," ^ (gen_init_decl
                (InitDeclList(t))))
   | InitDeclaratorAsn(decl, asn_op, expr) -> gen_decl decl ^ gen_assn_op asn_op
   ^ gen_expr expr

let rec gen_type typ = match typ with 
   | PrimitiveType(type_spec) -> gen_type_spec type_spec
   | CustomType(s) -> s

let rec gen_decl_specs ds = match ds with 
   | DeclSpecTypeSpec(ts) -> gen_type_spec ts
   | DeclSpecTypeSpecAny(typ) -> gen_type typ
   | DeclSpecTypeSpecInitList(typ, decls) -> gen_type typ ^ " " ^
   (gen_decl_specs decls)

let gen_declaration decl = match decl with 
   | Declaration(ds, init_decl) -> gen_decl_specs ds ^ " " ^ gen_init_decl
   init_decl

let gen_func_param p = match p with 
   | FuncParamsDeclared(ds, decl) -> gen_decl_specs ds ^ " " ^ gen_decl decl
   | ParamDeclWithType(ds) -> gen_decl_specs ds

let rec gen_func_params plist = match plist with 
   | [] -> ""
   | [p] -> gen_func_param p
   | h :: t -> gen_func_param h ^ ", " ^ (gen_func_params t)

let rec gen_statement stmt = match stmt with 
   | Expr(expr) -> gen_expr expr ^ ";"
   | If (e1, st, EmptyElse) -> "if(" ^ (gen_expr e1) ^ "){" ^
   gen_statement st ^ "}"
   | If (e1, st, st2) -> "if(" ^ (gen_expr e1) ^ ")" ^ gen_statement st ^ 
            "\nelse " ^ gen_statement st2
   | Return (expr) -> "return " ^ (gen_expr expr) ^ ";"
   | While (e1, st) -> "while(" ^ (gen_expr e1) ^ ")" ^ gen_statement st ^ "\n"
   | CompoundStatement(decls, stmts) -> (match decls with 
             | [] -> "{" ^ (String.concat ";\n" (List.map gen_statement
             stmts)) ^ "}"
             | _ ->  "{" ^ (String.concat ";\n" (List.map
   gen_declaration decls)) ^ ";" ^ (String.concat "\n" (List.map gen_statement
   stmts)) ^ "}")
   | Break -> "break;\n" 
 
let gen_func fdecl = (gen_decl_specs fdecl.return_type) ^ " " ^ (gen_decl
fdecl.func_name) ^ "(" ^ (gen_func_params fdecl.params) ^ ")" ^ (gen_statement
fdecl.body)

let gen_program program = 
        add_header "stdio" ^ "\n" ^ (String.concat ";\n" (List.map gen_declaration
        program.globals)) ^ ";\n\n" ^ (String.concat "\n" (List.map gen_func
        (List.rev program.functions)));

