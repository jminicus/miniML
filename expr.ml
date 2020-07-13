(*
                         CS 51 Final Project
                        MiniML -- Expressions
*)

(*......................................................................
  Abstract syntax of MiniML expressions
 *)

type unop =
  | Negate
  | Length
;;

type binop =
  | Plus
  | Minus
  | Times
  | Equals
  | LessThan
  | Concat
;;

type varid = string ;;

type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Bool of bool                         (* booleans *)
  | String of string                     (* STRING *)
  | Float of float                       (* FLOAT *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
  | Try of expr * expr                   (* TRY WITH STATEMENTS *)
;;

(*......................................................................
  Manipulation of variable names (varids)
 *)

(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                      end ) ;;

type varidset = SS.t ;;

(* same_vars :  varidset -> varidset -> bool
   Test to see if two sets of variables have the same elements (for
   testing purposes) *)
let same_vars : varidset -> varidset -> bool =
  SS.equal ;;

(* vars_of_list : string list -> varidset
   Generate a set of variable names from a list of strings (for
   testing purposes) *)
let vars_of_list : string list -> varidset =
  SS.of_list ;;

(* free_vars : expr -> varidset
   Return a set of the variable names that are free in expression
   exp *)
let rec free_vars (exp : expr) : varidset =
  match exp with
  | Var x -> SS.singleton x
  | Num _ | Bool _ | Raise | Unassigned | String _ | Float _ -> SS.empty
  | Unop (_, e) -> free_vars e
  | Binop (_, e1, e2) | App (e1, e2) -> SS.union (free_vars e1) (free_vars e2)
  | Conditional (e1, e2, e3) ->
      SS.union (SS.union (free_vars e1) (free_vars e2)) (free_vars e3)
  | Fun (v, e) -> SS.remove v (free_vars e)
  | Let (v, e1, e2) -> SS.union (SS.remove v (free_vars e2)) (free_vars e1)
  | Letrec (v, e1, e2) ->
      SS.union (SS.remove v (free_vars e1)) (SS.remove v (free_vars e2))
  | Try (e1, e2) -> SS.union (free_vars e1) (free_vars e2) ;;

(* new_varname : unit -> varid
   Return a fresh variable, constructed with a running counter a la
   gensym. Assumes no variable names use the prefix "var". (Otherwise,
   they might accidentally be the same as a generated variable name.) *)
let new_varname : unit -> varid =
  let ctr = ref 0 in
  fun () ->
    let new_v = "x" ^ string_of_int !ctr in
    ctr := !ctr + 1 ;
  new_v ;;

(*......................................................................
  Substitution

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)


(* subst : varid -> expr -> expr -> expr
   Substitute repl for free occurrences of var_name in exp *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  let rec subst' (exp : expr) =

    (* if the varid is var_name, then dont substitute. else if the varid is
       already part of the free variable set, then create a new variable
       and substitute that in. else if the variable is new and not var_name,
       use it and substitute the expression *)
    let decide (varid : varid) (exp1 : expr) (exp2 : expr) (exp3 : expr) =
      if varid = var_name then exp1
      else if SS.mem varid (free_vars repl) then exp2
      else exp3 in

    match exp with
    | Var x -> if x = var_name then repl else exp
    | Num _ | Bool _ | String _ | Float _ -> exp
    | Unop (neg, e) -> Unop (neg, subst' e)
    | Binop (bin, e1, e2) -> Binop (bin, subst' e1, subst' e2)
    | Conditional (e1, e2, e3) -> Conditional (subst' e1, subst' e2, subst' e3)
    | Fun (v1, e) -> decide v1 exp
        (let v2 = new_varname () in Fun (v2, subst' (subst v1 (Var v2) e)))
        (Fun (v1, subst' e))
    | Let (v1, e1, e2) -> decide v1 (Let (v1, subst' e1, e2))
        (let v2 = new_varname () in
         Let (v2, subst' e1, subst' (subst v1 (Var v2) e2)))
        (Let (v1, subst' e1, subst' e2))
    | Letrec (v1, e1, e2) -> decide v1 exp
        (let v2 = new_varname () in
         Letrec (v2, subst' (subst v1 (Var v2) e1),
                 subst' (subst v1 (Var v2) e2)))
        (Letrec (v1, subst' e1, subst' e2))
    | Unassigned | Raise -> exp
    | App (e1, e2) -> App (subst' e1, subst' e2)
    | Try (e1, e2) -> Try (subst' e1, subst' e2) in
  subst' exp ;;

(*......................................................................
  String representations of expressions
 *)

(* exp_to_concrete_string : expr -> string
   Returns a concrete syntax string representation of the expr *)
let rec exp_to_concrete_string (exp : expr) : string =
  let etcs = exp_to_concrete_string in
  let let_rec_format beginning varid e1 e2 =
    beginning ^ varid ^ " = " ^ (etcs e1) ^ " in " ^ (etcs e2) in
  match exp with
  | Var x | String x -> x
  | Num n -> string_of_int n
  | Bool b -> string_of_bool b
  | Float f -> string_of_float f
  | Unop (unop, e) ->
      let unop_format beginning e =
        beginning ^ (etcs e) ^ ")" in
      (match unop with
       | Negate -> unop_format ("~-(") e
       | Length -> unop_format ("#(") e)
  | Binop (bin, e1, e2) ->
      (let bin_format bin = (etcs e1) ^ bin ^ (etcs e2) in
        match bin with
        | Plus -> bin_format " + "
        | Minus -> bin_format " - "
        | Times -> bin_format " * "
        | Equals -> bin_format " = "
        | LessThan -> bin_format " < "
        | Concat -> bin_format " ^ ")
  | Conditional (e1, e2, e3) ->
      "if " ^ (etcs e1) ^ " then " ^ (etcs e2) ^ " else " ^ (etcs e3)
  | Fun (v, e) -> "fun " ^ v ^ " -> " ^ (etcs e)
  | Let (v, e1, e2) -> let_rec_format "let " v e1 e2
  | Letrec (v, e1, e2) -> let_rec_format "let rec " v e1 e2
  | Unassigned -> "Unassigned"
  | Raise -> "raise"
  | App (e1, e2) -> (etcs e1) ^ " " ^ (etcs e2)
  | Try (e1, e2) -> "try " ^ (etcs e1) ^ " with " ^ (etcs e2) ;;

(* exp_to_abstract_string : expr -> string
   Returns a string representation of the abstract syntax of the expr *)
let rec exp_to_abstract_string (exp : expr) : string =
  let etas = exp_to_abstract_string in
  let format_simple operation value =
    operation ^ "(" ^ value ^ ")" in
  let let_rec_format beginning v e1 e2 =
    beginning ^ "(" ^ v ^ ", " ^ (etas e1) ^ ", " ^ (etas e2) ^ ")" in
  match exp with
  | Var x -> format_simple "Var" x
  | Num n -> format_simple "Num" (string_of_int n)
  | Bool b -> format_simple "Bool" (string_of_bool b)
  | String s -> format_simple "String" s
  | Float f -> format_simple "Float" (string_of_float f)
  | Unop (unop, e) ->
      (match unop with
      | Negate -> format_simple "Unop(Negate, " (etas e)
      | Length -> format_simple "Unop(Length, " (etas e))
  | Binop (bin, e1, e2) ->
      (let format_bin bin =
         "Binop(" ^ bin ^ ", " ^ (etas e1) ^ ", " ^ (etas e2) ^ ")" in
      match bin with
      | Plus -> format_bin "Plus"
      | Minus -> format_bin "Minus"
      | Times -> format_bin "Times"
      | Equals -> format_bin "Equals"
      | LessThan -> format_bin "LessThan"
      | Concat -> format_bin "Concat")
  | Conditional (e1, e2, e3) -> format_simple "Conditional" ((etas e1) ^ ", " ^
                                (etas e2) ^ ", " ^ (etas e3))
  | Fun (v, e) -> format_simple "Fun" (v ^ ", " ^ (etas e))
  | Let (v, e1, e2) -> let_rec_format "Let" v e1 e2
  | Letrec (v, e1, e2) -> let_rec_format "Letrec" v e1 e2
  | Unassigned -> "Unassigned"
  | Raise -> "Raise"
  | App (e1, e2) -> format_simple "App" ((etas e1) ^ ", " ^ (etas e2))
  | Try (e1, e2) -> format_simple "Try(" ((etas e1) ^ ", " ^ (etas e2)) ;;
