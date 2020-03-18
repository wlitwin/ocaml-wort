open! Core

module Var = struct
    type t = IVar of string
           | SVar of string
        [@@deriving show]

    let pretty_string = function
        | IVar s -> s
        | SVar s -> s ^ "..."
    ;;

    let name = function
        | IVar s -> s
        | SVar s -> s
    ;;

    let equal a b =
        match a, b with
        | SVar s1, SVar s2 -> String.equal s1 s2
        | IVar s1, IVar s2 -> String.equal s1 s2
        | _ -> false
    ;;

    let equal_ignore a b =
        String.equal (name a) (name b)
    ;;
end

module Name = struct
    let index = ref 0

    let reset_var () = index := 0

    let fresh_var ?(letter="a") () =
        let v = letter ^ Int.to_string !index in
        incr index;
        v

    let fresh_ind () = 
        Var.IVar (fresh_var())

    let fresh_seq () =
        Var.SVar (fresh_var ~letter:"p" ())
end

module Type = struct

    type t = TInt
           | TBool
           | Var of Var.t 
           | Fun of { in_ : t; out: t }
           | List of t list
        [@@deriving show]

    type scheme = Scheme of Var.t list * t

    let rec pretty_string = function
        | Var (IVar s) -> s
        | Var (SVar s) -> s ^ "..."
        | Fun {in_; out} ->
            let pin = pretty_string in_
            and pout = pretty_string out in
            Printf.sprintf "(%s -> %s)" pin pout
        | TInt -> "int"
        | TBool -> "bool"
        | List lst -> 
            let lst = List.map ~f:pretty_string (List.rev lst) in
            String.concat ~sep:" " lst
    ;;

    let pretty_scheme (Scheme (vars, t)) =
        Printf.sprintf "forall %s. %s"
            String.(concat ~sep:", " List.(map ~f:Var.pretty_string vars))
            (pretty_string t)
    ;;

    let get_scheme_vnames (Scheme (vars, _)) =
        List.map ~f:Var.name vars

    module Env = struct
        type t = (string * scheme) list

        let extend (n : string) (t : scheme) (env : t) : t =
            (n, t) :: env
        ;;

        let lookup (t : t) (s : string) =
            try
                List.Assoc.find_exn ~equal:String.equal t s
            with e ->
                print_endline ("Failed to find " ^ s);
                raise e
        ;;
            
        let empty : t = []
    end

    module Subst = struct
        type item = string * t
        [@@deriving show]
        type subst = item list
        [@@deriving show]

        let empty : subst = []

        let find (t : subst) s =
            List.Assoc.find ~equal:String.equal t s
        ;;

        let find_bool (t : subst) s =
            find t s |> Option.is_some
        ;;

        let not_in_scheme (strs : string list) (item : item) : bool =
            not List.(mem strs (fst item) ~equal:String.equal)
        ;;

        let as_var = function
            | Var v -> v
            | t -> failwith ("Expected var " ^ pretty_string t)
        ;;

        let lst_as_var l = l |> List.hd_exn |> as_var

        let rec flatten = function
            | [] -> []
            | (List l1 :: l2) -> (List.append (flatten l1) (flatten l2))
            | (t :: l2) -> t :: flatten l2

        let rec flatten_lists = function
            | List lst -> List (flatten lst)
            | Fun {in_; out} -> Fun {in_=flatten_lists in_; out=flatten_lists out}
            | t -> t

        let rec sub (s : subst) (t : t) : t =
            match t with
            | TInt | TBool -> t
            | Fun {in_; out} -> Fun {in_ = sub s in_; out = sub s out}
            | Var (SVar n)
            | Var (IVar n) -> 
                begin match find s n with
                | Some v -> v
                | None -> t
                end
            | List lst -> List (flatten (List.map ~f:(sub s) lst))
        ;;

        let sub_list s lst =
            List.map ~f:(sub s) lst
        ;;

        let sub_scheme (s : subst) (Scheme (vars, ty) as t : scheme) =
            let x = List.filter ~f:(not_in_scheme (get_scheme_vnames t)) s in
            Scheme (vars, sub x ty)
        ;;

        let sub_gamma (s : subst) (t : Env.t) : Env.t =
            List.map t ~f:(fun x -> 
                let fx = fst x in
                let sx = sub_scheme s (snd x) in
                fx, sx
            ) 
        ;;

        let compose (s1 : subst) (s2 : subst) : subst =
            let rec combine_subs (s1 : subst) (s2 : subst) : subst =
                match s2 with
                | [] -> s1
                | (v, t) :: s2s ->
                    if find_bool s1 v then
                        combine_subs s1 s2s
                    else
                        (v, t) :: (combine_subs s1 s2s)
            in
            let sub_in_type s (pair : item) =
                let name, ty = pair in
                name, sub s ty
            in
            combine_subs List.(map ~f:(sub_in_type s1) s2) s1
        ;;
    end

    let fun_in = function
        | Fun {in_; _} -> in_
        | _ -> failwith "fun_in: Expected fun"

    let fun_out = function
        |  Fun {out; _} -> out
        | _ -> failwith "fun_out: Expected fun"

    let rec ftv (t : t) : Var.t list =
        match t with
        | Var (SVar s) -> [Var.SVar s]
        | Var (IVar s) -> [Var.IVar s]
        | Fun {in_; out} -> List.append (ftv in_) (ftv out)
        | List lst -> List.join (List.map ~f:ftv lst)
        | _ -> []
    ;;

    let ftv_scheme (s : scheme) : Var.t list =
        match s with
        | Scheme (vs, mono) -> 
            List.filter ~f:(fun v -> 
                not (List.exists vs ~f:(fun var ->
                    Var.equal v var
                ))
            ) (ftv mono)
    ;;

    let ftv_list lst = List.join List.(map ~f:ftv lst)

    let ftv_gamma (g : Env.t) : Var.t list =
        List.join (List.map ~f:(function s -> ftv_scheme (snd s)) g)
    ;;

    let gen (env : Env.t) (t : t) : scheme =
        let env_ftv = ftv_gamma env in
        let vs = List.filter ~f:(fun x ->
            not (List.mem ~equal:Var.equal env_ftv x)
        ) (ftv t) in
        Scheme (vs, t)
    ;;

    let inst (sch : scheme) : t =
        let freshen = function
            | Var.SVar s -> s, Var (Name.fresh_seq())
            | Var.IVar s -> s, Var (Name.fresh_ind())
        in
        match sch with
        | Scheme (vs, t) ->
            let fresh = List.map ~f:freshen vs in
            Subst.sub fresh t
    ;;

    let rec flatten_ty_list : t list -> t list = function
        | [List lst] -> flatten_ty_list lst
        | t -> t
    ;;

    let rec unify_ind (t1 : t) (t2 : t) : Subst.subst =
        let member v lst = List.mem ~equal:Var.equal lst v in
        let t1 = Subst.flatten_lists t1
        and t2 = Subst.flatten_lists t2 in
        if Poly.equal t1 t2 then []
        else begin 
        match t1, t2 with
        | Var (IVar v1), t2p when not (member (IVar v1) (ftv t2p)) ->
                [v1, t2p]
        | t1p, Var (IVar v2) when not (member (IVar v2) (ftv t1p)) ->
                [v2, t1p]
        | Fun {in_=in1; out=out1}, Fun {in_=in2; out=out2} ->
                let phi = unify [in1] [in2] in
                let phi2 = unify [Subst.sub phi out1] [Subst.sub phi out2] in
                Subst.compose phi2 phi

        | left, right -> 
            let error = Printf.sprintf "Unification failure\nLeft: %s\nRight: %s\n"
                (pretty_string left)
                (pretty_string right)
            in
            failwith error
        end

    and unify (s1 : t list) (s2 : t list) : Subst.subst =
        let s1 = flatten_ty_list s1
        and s2 = flatten_ty_list s2 in
        let check_list var ts =
            let lst = ftv_list ts in
            not (List.mem ~equal:Poly.equal lst var)
        in
        let not_svars v1 v2 =
            match v1, v2 with
            | Var (SVar _), Var (SVar _) -> false
            | _ -> true
        in
        let ty_eq t1 t2 =
            Poly.equal t1 t2
        in
        match s1, s2 with
        | [], [] -> []
        | t1 :: t1s, t2 :: t2s when ty_eq t1 t2 -> 
                unify t1s t2s
        | [Var (SVar v1)], t2s when check_list (SVar v1) t2s ->
                [v1, List t2s]
        | t1s, [Var (SVar v2)] when check_list (SVar v2) t1s ->
                [v2, List t1s]
        | t1 :: t1s, t2 :: t2s when not_svars t1 t2 ->
                let phi = unify_ind t1 t2 in
                let phi2 = unify (Subst.sub_list phi t1s) (Subst.sub_list phi t2s) in
                Subst.compose phi2 phi
        | _ -> failwith "Failed to unify"
    ;;

    let rec infer_prim (p : Wort.prim) =
        match p with
        | Mul
        | Div
        | Sub
        | Add -> 
            let a = Name.fresh_seq() in
            Fun {in_ = List [TInt; TInt; Var a]; out = List [TInt; Var a]}

        | Call ->
            let a = Name.fresh_seq()
            and b = Name.fresh_seq() in
            Fun {in_ = List [Fun {in_ = Var a; out = Var b}; Var a]; out = Var b}

        | Fix ->
            let a = Name.fresh_seq()
            and b = Name.fresh_seq() in
            Fun {in_ = 
                List [
                    Fun {in_ = List [Fun {in_ = Var a; out = Var b}; 
                                     Var a
                                    ]; 
                         out = List[Var b]};
                    Var a
                ];
                out = List[Var b]}
        | If -> 
            let a = Name.fresh_seq() in
            let b = Name.fresh_ind() in
            Fun {in_ = List [TBool; Var b; Var b; Var a]; out = List [Var b; Var a]}

        | Eq ->
            let a = Name.fresh_seq() in
            Fun {in_ = List [TInt; TInt; Var a]; out = List [TBool; Var a]}

        | Less ->
            let a = Name.fresh_seq() in
            Fun {in_ = List [TInt; TInt; Var a]; out = List [TBool; Var a]}

    and infer_word (env : Env.t) (word : Wort.word) : Subst.subst * t =
        match word with
        | Int _ -> 
                let a = Name.fresh_seq() in
                Subst.empty, Fun {in_ = Var a; out = List [TInt; Var a]}
        | Bool _ ->
                let a = Name.fresh_seq() in
                Subst.empty, Fun {in_ = Var a; out = List [TBool; Var a]}

        | Block e ->
                let r1 = infer env List.(rev e) in
                let s1, t1 = r1 in
                let a = Name.fresh_seq() in
                s1, Fun {in_ = Var a; out = List [t1; Var a]}

        | Prim n ->
                Subst.empty, infer_prim n

        | Var c ->
                let v = Env.lookup env c |> inst in
                Subst.empty, v

        | Bind (n, e) ->
                let a = Name.fresh_seq() in
                let b = Name.fresh_ind() in
                let new_env = Env.extend n (Scheme ([a], Fun {in_ = Var a; out = List [Var b; Var a]})) env in
                let r1 = infer new_env (List.rev e) in
                let s1, t1 = r1 in
                let subs = Subst.sub s1 (Var b) in
                s1, Fun {in_ = List [subs; fun_in t1]; out = fun_out t1}

        | Let (n, e1, e2) ->
                let r1 = infer env (List.rev e1) in
                let s1, t1 = r1 in
                let envp = Subst.sub_gamma s1 env in
                let r2 = infer Env.(extend n (gen envp t1) envp) (List.rev e2) in
                let s2, t2 = r2 in
                Subst.compose s2 s1, t2

    and infer (env : Env.t) (expr : Wort.expr) : Subst.subst * t =
        match expr with
        | [] -> 
            let a = Name.fresh_seq() in
            Subst.empty, Fun {in_=Var a; out=Var a}
        | w :: e ->
             let r1 = infer env e in
             let s1, t1 = r1 in
             let t1 = Subst.flatten_lists t1 in
             let r2 = infer_word (Subst.sub_gamma s1 env) w in
             let s2, t2 = r2 in
             let t2 = Subst.flatten_lists t2 in
             let phi = unify [fun_out t1] [fun_in t2] in
             Subst.compose (Subst.compose phi s2) s1,
             Subst.sub phi (Fun {in_=fun_in t1; out=fun_out t2}) |> Subst.flatten_lists
        ;;
end

let check_type expr =
    try
       Name.reset_var();
       let t = Type.infer Type.Env.empty expr |> snd in
       Printf.printf "inferred type: %s\n" (Type.pretty_string t) 
    with (Failure error) ->
        print_endline error;
        Backtrace.(Exn.most_recent() |> to_string |> print_endline);
        failwith "Typechecking failed"
;;

