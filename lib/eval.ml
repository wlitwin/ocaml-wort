open! Core
open Wort

type machine = {
    stack : Wort.word list;
    expr : Wort.expr;
}

let do_arith f = function
    | Int i1, Int i2 -> Int (f i1 i2)
    | _ -> failwith "Expected integers"

let do_add = do_arith ( + )
let do_mul = do_arith ( * )
let do_sub = do_arith ( - )
let do_div = do_arith ( / )

let do_eq = function
    | Int i1, Int i2 -> if i1 = i2 then Bool true else Bool false
    | _ -> failwith "Eq - expected integers"

let do_less = function
    | Int i1, Int i2 -> if i1 < i2 then Bool true else Bool false
    | _ -> failwith "Less - expected integers"

let dump_state {expr; stack} =
    let expr = Wort.show_expr expr in
    let stack = Wort.show_expr stack in
    Printf.printf 
{|
EXPR
=====
%s

STACK
=====
%s
|}
expr
stack
;;

let eval (expr : Wort.expr) =
    let rec step_eval (state : machine) =
        if List.is_empty state.expr then
            state.stack
        else step_eval (step state)

    and step (state : machine) =
        match state with
        (* Value pushing *)
        | {stack; expr = Int _ as i :: expr} -> {stack = i :: stack; expr}
        | {stack; expr = Bool _ as b :: expr} -> {stack = b :: stack; expr}
        | {stack; expr = Block _ as i :: expr} -> {stack = i :: stack; expr}

        (* Value Binding *)
        | {stack = v :: s; expr = Bind (name, body) :: expr} ->
            {stack = s; expr = List.append (substitute name [v] body) expr}

        (* Let Binding *)
        | {stack; expr = Let (name, arg, body) :: expr} ->
            {stack; expr = List.append (substitute name arg body) expr}

        (* Primitives *)
        | {stack = n1 :: n2 :: s; expr = Prim Add :: expr} ->
            {stack = do_add (n1, n2) :: s; expr}

        | {stack = n1 :: n2 :: s; expr = Prim Mul :: expr} ->
            {stack = do_mul (n1, n2) :: s; expr}

        | {stack = n1 :: n2 :: s; expr = Prim Div :: expr} ->
            {stack = do_div (n1, n2) :: s; expr}

        | {stack = n1 :: n2 :: s; expr = Prim Sub :: expr} ->
            {stack = do_sub (n1, n2) :: s; expr}

        | {stack = n1 :: n2 :: s; expr = Prim Eq :: expr} ->
            {stack = do_eq (n1, n2) :: s; expr}

        | {stack = n1 :: n2 :: s; expr = Prim Less :: expr} ->
            {stack = do_less (n1, n2) :: s; expr}

        | {stack = Block exp :: s; expr = Prim Call :: e} ->
            {stack = s; expr = List.append exp e}

        | {stack = Block exp :: s; expr = Prim Fix :: e} ->
            {stack = Block [Block exp; Prim Fix] :: s; expr = List.append exp e}

        | {stack = Bool true :: v1 :: _v2 :: s; expr = Prim If :: expr} ->
            {stack = v1 :: s; expr}

        | {stack = Bool false :: _v1 :: v2 :: s; expr = Prim If :: expr} ->
            {stack = v2 :: s; expr}

        | machine -> 
                dump_state machine;
                failwith "Got stuck"
    in

    step_eval {stack = []; expr}
;;
