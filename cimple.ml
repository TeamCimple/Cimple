open Codegen

type tAction = Ast | Compile 

let _ =

       let action = if Array.length Sys.argv > 1 then
        List.assoc Sys.argv.(1) [ ("-a", Ast);      (* Print the AST only *)
                              ("-c", Compile) ] (* Generate, check LLVM IR *)
       else Compile in

       let lexbuf = Lexing.from_channel stdin in
       let program = Parser.program Scanner.token lexbuf in
       match action with
                Ast -> Printf.printf "%s\n" (Astutil.string_of_program program)
              | Compile -> 
                       (Semant.check_program program;
                       Printf.printf "%s\n" (Codegen.gen_program program))
