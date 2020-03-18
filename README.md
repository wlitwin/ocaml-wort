OCaml Wort
====

This is a port of Robert Kleffner's (Wort language)[https://github.com/robertkleffner/wort] from Racket to OCaml with a few modifications. The port tries to stay close to the Racket implementation, notably `eval.ml` and `infer.ml`

The modifications are in the syntax. Instead of:

```
let name = bind x (bind y (
   expr
)) (
   1 2 name
)
```

You can use:

```
let name x y =
    expr
in
1 2 name
```

It also adds a couple extra primitives for testing sake, like `mul`, and `sub`.

Building
====

Simply running `make` should build the project. You will need to have OCaml installed along with:

`menhir` `core` `dune` `ocamllex`

Examples
====
The `examples/` directory contains some examples. Not all of them are valid programs. Some will fail to typecheck.
