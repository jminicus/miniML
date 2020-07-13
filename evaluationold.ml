(*
                         CS 51 Final Project
                         MiniML -- Evaluation
*)

(* This module implements a small untyped ML-like language under
   various operational semantics.
 *)

open Expr ;;

(* Exception for evaluator runtime, generated by a runtime error in
   the interpreter *)
exception EvalError of string ;;

(* Exception for evaluator runtime, generated by an explicit `raise`
   construct in the object language *)
exception EvalException ;;

(*......................................................................
  Environments and values
 *)

module type ENV = sig
    (* the type of environments *)
    type env
    (* the type of values stored in environments *)
    type value =
      | Val of expr
      | Closure of (expr * env)

    (* empty () -- Returns an empty environment *)
    val empty : unit -> env

    (* close expr env -- Returns a closure for `expr` and its `env` *)
    val close : expr -> env -> value

    (* lookup env varid -- Returns the value in the `env` for the
       `varid`, raising an `Eval_error` if not found *)
    val lookup : env -> varid -> value

    (* extend env varid loc -- Returns a new environment just like
       `env` except that it maps the variable `varid` to the `value`
       stored at `loc`. This allows later changing the value, an
       ability used in the evaluation of `letrec`. To make good on
       this, extending an environment needs to preserve the previous
       bindings in a physical, not just structural, way. *)
    val extend : env -> varid -> value ref -> env

    (* env_to_string env -- Returns a printable string representation
       of environment `env` *)
    val env_to_string : env -> string

    (* value_to_string ?printenvp value -- Returns a printable string
       representation of a value; the optional flag `printenvp`
       (default: `true`) determines whether to include the environment
       in the string representation when called on a closure *)
    val value_to_string : ?printenvp:bool -> value -> string

    val extract_exp : value -> expr
  end

module Env : ENV =
  struct
    type env = (varid * value ref) list
     and value =
       | Val of expr
       | Closure of (expr * env)

    let empty () : env = [] ;;

    let close (exp : expr) (env : env) : value =
      Closure (exp, env) ;;

    let lookup (env : env) (varname : varid) : value =
      try
        !(List.assoc varname env)
      with
      | Not_found -> raise (EvalError ("Variable " ^ varname ^ " is unbound"))

    let rec extend (env : env) (varname : varid) (loc : value ref) : env =
      match env with
      | [] -> [varname, loc]
      | (v, vref) :: tl -> if v = varname then (v, loc) :: tl
                           else (v, vref) :: extend tl varname loc

    let rec value_to_string ?(printenvp : bool = true) (v : value) : string =
      match v with
      | Val exp -> exp_to_concrete_string exp
      | Closure (exp, env) ->
        if printenvp then
          (env_to_string env) ^ " || " ^ (exp_to_concrete_string exp)
        else exp_to_concrete_string exp

    and env_to_string (env : env) : string =
      match env with
      | [] -> ""
      | (v, vref) :: tl -> v ^ " = " ^ (value_to_string ~printenvp:false !vref)
                           ^ "\n" ^ (env_to_string tl)

    (* simple helper for extracting expression out of Env.value *)
    let extract_exp (value : value) : expr =
      match value with
      | Val expr -> expr
      | Closure (expr, _) -> expr
  end ;;


(*......................................................................
  Evaluation functions

  Each of the evaluation functions below evaluates an expression `exp`
  in an enviornment `env` returning a result of type `value`. We've
  provided an initial implementation for a trivial evaluator, which
  just converts the expression unchanged to a `value` and returns it,
  along with "stub code" for three more evaluators: a substitution
  model evaluator and dynamic and lexical environment model versions.

  Each evaluator is of type `expr -> Env.env -> Env.value` for
  consistency, though some of the evaluators don't need an
  environment, and some will only return values that are "bare
  values" (that is, not closures).

  DO NOT CHANGE THE TYPE SIGNATURES OF THESE FUNCTIONS. Compilation
  against our unit tests relies on their having these signatures. If
  you want to implement an extension whose evaluator has a different
  signature, implement it as `eval_e` below.  *)

(* The TRIVIAL EVALUATOR, which leaves the expression to be evaluated
   essentially unchanged, just converted to a value for consistency
   with the signature of the evaluators. *)

let eval_t (exp : expr) (_env : Env.env) : Env.value =
  (* coerce the expr, unchanged, into a value *)
  Env.Val exp ;;

(* The SUBSTITUTION MODEL evaluator *)
let rec eval_s (exp : expr) (_env : Env.env) : Env.value =

  (* recursively evaluates exp in an empty environment *)
  let rec eval_s' (exp : expr) : Env.value =
    match exp with
    | Var x -> raise (EvalError ("Variable " ^ x ^ " is unbound"))
    | Num _ | Bool _ | Fun _ | Unassigned -> Env.Val exp
    | Unop (_neg, e) ->
        (match eval_s' e with
        | Env.Val (Num n) -> Env.Val (Num (~- n))
        | _ -> raise (EvalError "Cannot negate a non-integer"))
    | Binop (bin, e1, e2) ->
        (match (eval_s' e1), (eval_s' e2) with
         | Env.Val (Num n1), Env.Val (Num n2) ->
             (match bin with
             | Plus -> Env.Val (Num (n1 + n2))
             | Minus -> Env.Val (Num (n1 - n2))
             | Times -> Env.Val (Num (n1 * n2))
             | Equals -> Env.Val (Bool (n1 = n2))
             | LessThan -> Env.Val (Bool (n1 < n2)))
         | _ -> raise (EvalError "Cannot do binary operations on non-numbers"))
    | Conditional (e1, e2, e3) ->
        if eval_s' e1 = Env.Val (Bool true) then eval_s' e2
        else if eval_s' e1 = Env.Val (Bool false) then eval_s' e3
        else raise (EvalError "Conditional does not use a bool")
    | Let (v, e1, e2) ->
        let sub_exp = subst v (Env.extract_exp (eval_s' e1)) e2 in
        eval_s' sub_exp
    | Letrec (v, e1, e2) ->
        let sub_exp = subst v (subst v (Letrec (v, e1, Var v)) e1) e2 in
        eval_s' sub_exp
    | Raise -> raise EvalException
    | App (e1, e2) ->
        match eval_s' e1 with
        | Env.Val Fun (v, e) ->
          let sub_exp = subst v (Env.extract_exp (eval_s' e2)) e in
          eval_s' sub_exp
        | _ -> raise (EvalError "Application must be to a function") in

  eval_s' exp ;;

(* The DYNAMICALLY-SCOPED ENVIRONMENT MODEL evaluator -- to be
   completed *)

let rec eval_d (exp : expr) (env : Env.env) : Env.value =

  (* recursively evaludates exp by updating the env where needed *)
  match exp with
  | Var x -> eval_d (Env.extract_exp (Env.lookup env x)) env
  | Num _ | Bool _ | Fun _ -> Env.Val exp
  | Unop (_neg, e) ->
      (match eval_d e env with
      | Env.Val (Num n) -> Env.Val (Num (~- n))
      | _ -> raise (EvalError "Cannot negate a non-integer"))
  | Binop (bin, e1, e2) ->
      (match (eval_d e1 env), (eval_d e2 env) with
       | Env.Val (Num n1), Env.Val (Num n2) ->
           (match bin with
            | Plus -> Env.Val (Num (n1 + n2))
            | Minus -> Env.Val (Num (n1 - n2))
            | Times -> Env.Val (Num (n1 * n2))
            | Equals -> Env.Val (Bool (n1 = n2))
            | LessThan -> Env.Val (Bool (n1 < n2)))
       | _ -> raise (EvalError "Cannot do binary operations on non-numbers"))
  | Conditional (e1, e2, e3) ->
      let cond = eval_d e1 env in
      if cond = Env.Val (Bool true) then eval_d e2 env
      else if cond = Env.Val (Bool false) then eval_d e3 env
      else raise (EvalError "Conditional does not use a bool")
  | Let (v, e1, e2) ->
      let e1_eval = Env.extract_exp (eval_d e1 env) in
      let new_env = Env.extend env v (ref (Env.close e1_eval env)) in
      eval_d e2 new_env
  | Letrec (v, e1, e2) ->
      let value = ref (Env.Val Unassigned) in
      let new_env = Env.extend env v value in
      value := Env.close (Env.extract_exp (eval_d e1 new_env)) env ;
      eval_d e2 new_env
  | Raise -> raise EvalException
  | Unassigned -> raise (EvalError "Unassigned variable")
  | App (e1, e2) ->
      match eval_d e1 env with
      | Env.Val Fun (v, e) ->
          let value = eval_d e2 env in
          let new_env = Env.extend env v
              (ref (Env.close (Env.extract_exp value) env)) in
          eval_d e new_env
      | _ -> raise (EvalError "Application must be to a function") ;;

(* The LEXICALLY-SCOPED ENVIRONMENT MODEL evaluator -- optionally
   completed as (part of) your extension *)

let eval_l (_exp : expr) (_env : Env.env) : Env.value =
  failwith "eval_l not implemented" ;;

(* The EXTENDED evaluator -- if you want, you can provide your
   extension as a separate evaluator, or if it is type- and
   correctness-compatible with one of the above, you can incorporate
   your extensions within `eval_s`, `eval_d`, or `eval_l`. *)

let eval_e _ =
  failwith "eval_e not implemented" ;;

(* Connecting the evaluators to the external world. The REPL in
   `miniml.ml` uses a call to the single function `evaluate` defined
   here. Initially, evaluate is the trivial evaluator `eval_t`. But
   you can define it to use any of the other evaluators as you proceed
   to implement them. (We will directly unit test the four evaluators
   above, not the evaluate function, so it doesn't matter how it's set
   when you submit your solution.) *)

let evaluate = eval_t ;;
