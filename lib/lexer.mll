{
    open Lexing
    open Parser

    exception SyntaxError of string

    let next_line lexbuf =
        let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <- {
            pos with pos_bol = lexbuf.lex_curr_pos;
                     pos_lnum = pos.pos_lnum + 1;
        }
}

let int = '-'? ['0'-'9'] ['0'-'9']*
let digit = ['0'-'9']
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read =
    parse
    | white { read lexbuf }
    | newline { next_line lexbuf; read lexbuf }
    | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | "(*" { comment 0 lexbuf }
    | "true" { TRUE } 
    | "false" { FALSE }
    | "if" { IF }
    | "add" { ADD }
    | "sub" { SUB }
    | "mul" { MUL }
    | "div" { DIV }
    | "less" { LESS }
    | "fix" { FIX }
    | "call" { CALL }
    | "let" { LET }
    | "bind" { BIND }
    | "eq" { EQ }
    | "in" { IN }
    | "=" { EQ_SYM }
    | '{' { LEFT_BRACE }
    | '}' { RIGHT_BRACE }
    | '(' { LEFT_PAREN }
    | ')' { RIGHT_PAREN }
    | id { VAR_NAME (Lexing.lexeme lexbuf) }
    | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
    | eof { EOF }
and comment level = parse
    | "(*" { comment (level + 1) lexbuf }
    | "\n" { next_line lexbuf; comment level lexbuf }
    | "*)" { if level = 0 then read lexbuf else comment (level - 1) lexbuf }
    | _    { comment level lexbuf }
    | eof  { raise (SyntaxError "Unterminated comment") }

