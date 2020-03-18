open! Core

type prim = Add
          | Mul
          | Div
          | Sub
          | Less
          | Eq
          | Call
          | If
          | Fix
	      [@@deriving show]

type word =
    | Prim of prim
    | Int of int
    | Bool of bool
    | Var of string
    | Block of expr
    | Bind of string * expr
    | Let of string * expr * expr
	[@@deriving show]

and expr = word list
	[@@deriving show]

let rec pretty_print = 
    let pretty_list lst = 
        lst
        |> List.map ~f:pretty_print
        |> String.concat ~sep:" "
    in
    function
    | Prim Add -> "add"
    | Prim Sub -> "sub"
    | Prim Mul -> "mul"
    | Prim Div -> "div"
    | Prim Less -> "less"
    | Prim Eq -> "eq"
    | Prim Call -> "call"
    | Prim If -> "if"
    | Prim Fix -> "fix"
    | Int i -> Int.to_string i
    | Var v -> v
    | Bool b -> Bool.to_string b
    | Block e -> "{" ^ pretty_list e ^ "}"
    | Bind (n, e) -> "bind " ^ n ^ " (" ^ pretty_list e ^ ")"
    | Let (n, e, b) ->
        "let " ^ n ^ " =\n" ^
            pretty_list e ^ "\nin\n" ^
            pretty_list b
;;

let pretty_expr e =
    e |> List.map ~f:pretty_print |> String.concat ~sep:"\n"
;;

let build_let_expr (names : string list) (e : expr) (body : expr) =
    let rec build_binds e = function
        | [] -> failwith "Expected non-empty list"
        | [name] -> Bind (name, e)
        | hd :: tl -> Bind (hd, [build_binds e tl])
    in
    match names with
    | [name] -> Let (name, e, body)
    | name :: rest ->
        Let (name, [build_binds e rest], body)
    | _ -> failwith "Expected non-empty vars"
;;

let substitute (name : string) (rep : expr) (target : expr) =
    let rec aux = function
        | Prim _ as p -> [p]
        | Int _ as i -> [i]
        | Bool _ as b -> [b]
        | Var v when String.(v = name) -> rep
        | Var _ as v -> [v]
        | Block e -> [Block (flatten_sub e)]
        | Bind (v, body) -> 
            let body = if String.(name = v) then body else (flatten_sub body) in
            [Bind (v, body)]
        | Let (v, arg, body) ->
            let arg = flatten_sub arg in
            let body = if String.(name = v) then body else (flatten_sub body) in
            [Let (v, arg, body)]
    and flatten_sub lst =
        lst |> List.map ~f:aux |> List.join
    in
    flatten_sub target
;;
