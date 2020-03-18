%token <int> INT
%token <string> VAR_NAME
%token ADD
%token MUL
%token SUB
%token DIV
%token IN
%token CALL
%token FIX
%token IF
%token TRUE
%token FALSE
%token EQ
%token LESS
%token LEFT_BRACE
%token RIGHT_BRACE
%token BIND
%token LEFT_PAREN
%token RIGHT_PAREN
%token EQ_SYM
%token LET
%token EOF

%start <Wort.expr option> prog

%%

prog:
    | EOF { None }
    | e = expr+; EOF { Some e }

prim:
    | ADD { Wort.Add }
    | SUB { Wort.Sub }
    | MUL { Wort.Mul }
    | DIV { Wort.Div }
    | CALL { Wort.Call }
    | FIX { Wort.Fix }
    | IF { Wort.If }
    | EQ { Wort.Eq }
    | LESS { Wort.Less }

expr:
    | p = prim; { Wort.Prim p }
    | i = INT; { Wort.Int i }
    | TRUE; { Wort.Bool true }
    | FALSE; { Wort.Bool false }
    | v = VAR_NAME { Wort.Var v }
    | LEFT_BRACE; e = expr*; RIGHT_BRACE { Wort.Block e }
    | BIND; v = VAR_NAME; LEFT_PAREN; e = expr*; RIGHT_PAREN { Wort.Bind (v, e) }
(*    | LET; v = VAR_NAME; EQ_SYM; e = expr*; LEFT_PAREN; ebody = expr*; RIGHT_PAREN { Wort.Let (v, e, ebody) }*)
    | LET; names = VAR_NAME+; EQ_SYM; e = expr*; IN; ebody = expr* {
        Wort.build_let_expr names e ebody
    }

