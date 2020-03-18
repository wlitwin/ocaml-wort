open! Core
open Ocaml_wort

let repl =
    Command.basic_spec ~summary:"Start a REPL"
        Command.Spec.(
            empty
        )
    Wort_repl.repl_loop
;;

let run_files =
    Command.basic_spec ~summary:"Run files"
        Command.Spec.(
            empty 
            +> flag "-i" no_arg ~doc:"Ignore typechecking"
            +> (sequence ("filename" %: string) |> anon)
        )
    Wort_repl.run_files
;;

let () =
    Command.group ~summary:"WORT interpreter"
        [
            "repl", repl;
            "run", run_files
        ]
  |> Command.run
;;
