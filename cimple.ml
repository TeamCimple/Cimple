open Ast

let _ =
       let lexbuf = Lexing.from_channel stdin in
       let program = Parser.program Scanner.token lexbuf in
       Semant2.check_program program (*Printf.printf "%s\n"
       (Astutil.string_of_program program*)
