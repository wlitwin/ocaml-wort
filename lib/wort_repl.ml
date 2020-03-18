open! Core
open Lexer
open Lexing

let print_position outx lexbuf =
    let pos = lexbuf.lex_curr_p in
    fprintf outx "%s:%d:%d" pos.pos_fname
      pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
;;

let parse_with_error lexbuf =
    try Parser.prog Lexer.read lexbuf with
    | SyntaxError msg ->
        fprintf stderr "%a: %s\n" print_position lexbuf msg;
        None
    | Parser.Error ->
        fprintf stderr "%a: syntax error" print_position lexbuf;
        exit(-1)
;;

let token_to_str = function
    | Parser.ADD -> "add"
    | MUL -> "mul"
    | SUB -> "sub"
    | DIV -> "div"
    | LET -> "let"
    | BIND -> "bind"
    | TRUE -> "true"
    | IN -> "in"
    | FALSE -> "false"
    | LEFT_PAREN -> "("
    | RIGHT_PAREN -> ")"
    | LEFT_BRACE -> "{"
    | RIGHT_BRACE -> "}"
    | LESS -> "less"
    | FIX -> "fix"
    | IF -> "if"
    | EQ_SYM -> "="
    | EQ -> "eq"
    | EOF -> "<eof>"
    | CALL -> "call"
    | VAR_NAME var -> "var_name(" ^ var ^ ")"
    | INT i -> Int.to_string i
;;

let rec lex_input lexbuf =
    let token = Lexer.read lexbuf in
    print_endline (token_to_str token);
    match token with
    | EOF -> ()
    | _ -> lex_input lexbuf;
;;

let parse_and_print ignore_typecheck lexbuf =
  match parse_with_error lexbuf with
  | Some value ->
    (*print_endline Wort.(show_expr value);*)
    (*parse_and_print lexbuf;*)
    (*print_endline Wort.(pretty_expr value);*)
    if not ignore_typecheck then
        Infer.check_type List.(rev value);
    let result = Eval.eval value in
    print_endline "\nRESULT\n";
    print_endline Wort.(pretty_expr result)
  | None -> ()
;;

let rec repl_loop() = 
    let lexbuf = Lexing.from_channel In_channel.stdin in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "stdin" };
    parse_and_print false lexbuf;
    print_endline "------------------------";
    repl_loop()
;;

let exec_file ignore_typecheck filename =
      print_endline filename;
      let inx = In_channel.create filename in
      let lexbuf = Lexing.from_channel inx in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
      parse_and_print ignore_typecheck lexbuf;
      In_channel.close inx;
      print_endline "~~~~~~~~~~~~~~~~~~~~~~";
;;

let run_files ignore_typecheck filenames () =
    List.iter ~f:(exec_file ignore_typecheck) filenames
;;
