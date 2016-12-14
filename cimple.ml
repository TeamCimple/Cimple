open Ccodegen 

type tAction = Ast | Compile | AnonFuncTest 

let _ =

       let action = if Array.length Sys.argv > 1 then
        List.assoc Sys.argv.(1) [ ("-a", Ast);      (* Print the AST only *)
                                  ("-c", Compile);       (* Attempt compilation (default) *)
                                  ("-l", AnonFuncTest) ]     
       else Compile in

       let lexbuf = Lexing.from_channel stdin in
       let program = Parser.program Scanner.token lexbuf in
       match action with
                Ast -> Printf.printf "%s\n" (Astutil.string_of_program program)
              | Compile -> 
                        Semant.check_program program; (Printf.printf
                       "%s\n", "finished checking");
                       let cprogram = Ctree.cProgram_from_tProgram program in
                       Printf.printf "%s\n" (Ccodegen.gen_cprogram cprogram)

              | AnonFuncTest -> 
                      Printf.printf "\n\nPrinting test results for anonymous function\n";
                      Ccodegen.test_anon_defs program;
                      Printf.printf "\n\n"
