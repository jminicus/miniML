(*
                         CS 51 Final Project
                           MiniML -- Tests
*)

(* This file contains tests for all subsections of the final project.
   when run, it will print whether each test has passed or failed using
   functions from absbook.ml. *)

open Absbook ;;
open Expr ;;
open Evaluation ;;
open Miniml ;;

(* TESTS FOR EXPR.ML *)

let free_vars_tests () =
  let fv exp = free_vars (str_to_exp exp) in
  unit_test (fv "x;;" = vars_of_list ["x"]) "free_v var" ;
  unit_test (fv "1;;" = vars_of_list []) "free_v num" ;
  unit_test (fv "true;;" = vars_of_list []) "free_v bool" ;
  unit_test (fv "~-2;;" = vars_of_list []) "free_v unop with num" ;
  unit_test (fv "~-x;;" = vars_of_list ["x"]) "free_v unop with var" ;
  unit_test (fv "x + 2;;" = vars_of_list ["x"]) "free_v binop with var" ;
  unit_test (fv "x + y;;" = vars_of_list ["x"; "y"]) "free_v binop w 2 vars" ;
  unit_test (fv "if true then x else 2;;" = vars_of_list ["x"])
            "free_v cond true" ;
  unit_test (fv "fun x -> 2 + y;;" = vars_of_list ["y"])
            "free_v fun not applied" ;
  unit_test (fv "let y = 2 in x + x;;" = vars_of_list ["x"]) "free_v let exp" ;
  unit_test (fv "let rec x = fun y -> x in x;;" = vars_of_list [])
            "free_v letrec exp" ;
  unit_test (fv "fun x -> ~-(x) y;;" = vars_of_list ["y"]) "free_v app exp1" ;
  unit_test (fv "fun x -> ~-(x) 2;;" = vars_of_list []) "free_v app exp2" ;;

let new_varname_tests () =
  unit_test (new_varname () = "x0") "new_var 0" ;
  unit_test (new_varname () = "x1") "new_var 1" ;
  unit_test (new_varname () = "x2") "new_var 2" ;
  unit_test (new_varname () = "x3") "new_var 3" ;
  unit_test (new_varname () = "x4") "new_var 4" ;
  let _ = new_varname () in
  unit_test (new_varname () = "x6") "new_var run twice" ;;

let subst_tests () =
  unit_test (subst "x" (Var "test") Unassigned = Unassigned) "subst unassigned" ;
  unit_test (subst "x" (Var "test") Raise = Raise) "subst raise" ;
  unit_test (subst "x" (Var "test") (Var "x") = Var "test") "subst simple 1" ;
  unit_test (subst "x" (Var "y") (Var "x") = Var "y") "subst simple 2" ;
  unit_test (subst "x" (Num 2) (Binop (Plus, (Num 2), (Var "x"))) =
             Binop (Plus, Num 2, Num 2)) "subst binop 1" ;
  unit_test (subst "y" (Num 2) (Binop (Plus, (Num 2), (Var "x"))) =
             Binop (Plus, Num 2, Var "x")) "subst binop 2" ;
  unit_test (subst "y" (Num 2) (Binop (Plus, (Num 2), (Var "x"))) =
             Binop (Plus, Num 2, Var "x")) "subst binop 2" ;
  unit_test (subst "x" (Var "new") (Conditional ((Bool true), (Fun ("x",
            (Binop (Plus, Var "x", Var "y")))), (Var "x"))) =
             Conditional (Bool true, Fun ("x", Binop (Plus, Var "x", Var "y")),
             Var "new")) "subst conditional and fun" ;
  unit_test (subst "x" (Var "new") ((Let ("y", (Num 2), (Binop (Plus, (Var "x"),
            (Var "x")))))) = Let ("y", Num 2, Binop
            (Plus, Var "new", Var "new"))) "subst let with free twice" ;
  unit_test (subst "y" (Var "new") (App (Fun ("x", Unop (Negate, Var "x")),
            (Var "y"))) = App (Fun ("x", Unop (Negate, Var "x")), Var "new"))
            "subst app with free" ;
  unit_test (subst "y" ((Let ("x", (Num 2), (Binop (Plus, (Var "m"),
            (Var "x")))))) ((Let ("x", (Num 2), (Binop (Plus, (Var "y"),
            (Var "y")))))) = Let ("x", Num 2, Binop (Plus, Let ("x", Num 2,
             Binop (Plus, Var "m", Var "x")), Let ("x", Num 2, Binop
            (Plus, Var "m", Var "x"))))) "subst double let" ;
  unit_test (subst "x" (Var "x1") (Let ("x1", (Num 2), (Var "x1"))) =
             Let ("x11", Num 2, Var "x11")) "subst let with renaming" ;
  unit_test (subst "x" (Var "x1") (Let ("x1", (Num 2), (Var "x1"))) =
             Let ("x12", Num 2, Var "x12")) "subst let with renaming again" ;
  unit_test (subst "y" (Var "new") (Letrec (("x"), (Fun (("y"), (Num 2))),
            (Var "y"))) = Letrec ("x", Fun ("y", Num 2), Var "new"))
            "subst letrec" ;;

let exp_to_concrete_string_tests () =
  unit_test (exp_to_concrete_string (Unop (Negate, (Num 2)))
            = "~-(2)") "etcs unop" ;
  unit_test (exp_to_concrete_string (Unop (Negate, (Var "x")))
            = "~-(x)") "etcs unop w variable" ;
  unit_test (exp_to_concrete_string (Binop (Plus, (Var "x"), (Num 2)))
            = "x + 2") "etcs binop with var" ;
  unit_test (exp_to_concrete_string (Binop (Plus, (Var "x"), (Var "y")))
            = "x + y") "etcs binop with two vars" ;
  unit_test (exp_to_concrete_string (Conditional ((Bool true), (Var "x"),
            (Num 2))) = "if true then x else 2") "etcs cond true" ;
  unit_test (exp_to_concrete_string (Fun ("x", Binop (Plus, (Num 2),
            (Var "y")))) = "fun x -> 2 + y") "etcs fun two vars" ;
  unit_test (exp_to_concrete_string (Let ("y", (Num 2), (Binop (Plus, (Var "x"),
            (Var "x"))))) = "let y = 2 in x + x")
            "etcs let bin two vars" ;
  unit_test (exp_to_concrete_string (Letrec (("x"), (Fun (("x"), (Var "x"))),
            (Var "x"))) = "let rec x = fun x -> x in x")
            "etcs let rec two vars" ;
  unit_test (exp_to_concrete_string (App (Fun ("x", Unop (Negate, Var "x")),
            (Num 2))) = "fun x -> ~-(x) 2") "etcs app fun unop two vars" ;
  unit_test (exp_to_concrete_string (Unassigned) = "Unassigned")
            "etcs unassigned" ;
  unit_test (exp_to_concrete_string (Raise) = "raise") "etcs raise" ;;

let exp_to_abstract_string_tests () =
  unit_test (exp_to_abstract_string (Unop (Negate, (Num 2)))
            = "Unop(Negate, (Num(2))") "etas unop" ;
  unit_test (exp_to_abstract_string (Unop (Negate, (Var "x")))
            = "Unop(Negate, (Var(x))") "etas unop w variable" ;
  unit_test (exp_to_abstract_string (Binop (Plus, (Var "x"), (Num 2)))
            = "Binop(Plus, Var(x), Num(2))") "etas binop with var" ;
  unit_test (exp_to_abstract_string (Binop (Plus, (Var "x"), (Var "y")))
            = "Binop(Plus, Var(x), Var(y))") "etas binop with two vars" ;
  unit_test (exp_to_abstract_string (Conditional ((Bool true), (Var "x"),
            (Num 2))) = "Conditional(Bool(true), Var(x), Num(2))")
            "etas cond true" ;
  unit_test (exp_to_abstract_string (Fun ("x", Binop (Plus, (Num 2),
            (Var "y")))) = "Fun(x, Binop(Plus, Num(2), Var(y)))")
            "etas fun two vars" ;
  unit_test (exp_to_abstract_string (Let ("y", (Num 2), (Binop (Plus, (Var "x"),
            (Var "x"))))) = "Let(y, Num(2), Binop(Plus, Var(x), Var(x)))")
            "etas let bin two vars" ;
  unit_test (exp_to_abstract_string (Letrec (("x"), (Fun (("y"), (Var "x"))),
            (Var "x"))) = "Letrec(x, Fun(y, Var(x)), Var(x))")
            "etas let rec two vars" ;
  unit_test (exp_to_abstract_string (App (Fun ("x", Unop (Negate, Var "x")),
            (Var "y"))) = "App(Fun(x, Unop(Negate, (Var(x))), Var(y))")
            "etas app fun unop two vars" ;
  unit_test (exp_to_abstract_string (App (Fun ("x", Unop (Negate, Var "x")),
            (Num 2))) = "App(Fun(x, Unop(Negate, (Var(x))), Num(2))")
            "etas app fun unop one var" ;
  unit_test (exp_to_abstract_string (Unassigned) = "Unassigned")
            "etas unassigned" ;
  unit_test (exp_to_abstract_string (Raise) = "Raise") "etas raise" ;;

(* TEST FOR ENVIRONMENT MODULE *)

let env_tests () =
  let empty = Env.empty () in
  unit_test (Env.env_to_string empty = "") "env empty" ;
  let n1 = ref (Env.Val (Num 1)) in
  let env1 = Env.extend empty "x" n1 in
  unit_test (Env.env_to_string env1 = "x = 1\n") "env one value" ;
  let n2 = ref (Env.Val (Num 2)) in
  let env2 = Env.extend env1 "y" n2 in
  unit_test (Env.env_to_string env2 = "x = 1\ny = 2\n") "env two values" ;
  let n3 = ref (Env.Val (Num 3)) in
  let env3 = Env.extend env2 "x" n3 in
  unit_test (Env.env_to_string env3 = "x = 3\ny = 2\n") "env reassignment" ;
  let n4 = ref (Env.Val (Bool true)) in
  let env4 = Env.extend env3 "y" n4 in
  unit_test (Env.env_to_string env4 = "x = 3\ny = true\n")
            "env reassignment w bool" ;
  let n5 = ref (Env.Val ((Fun ("m", Binop (Plus, (Num 2), (Var "n")))))) in
  let env5 = Env.extend env4 "x" n5 in
  unit_test (Env.env_to_string env5 = "x = fun m -> 2 + n\ny = true\n")
            "env reassignment to fun" ;
  let lookup_x = Env.lookup env4 "x" in
  unit_test (Env.value_to_string ~printenvp:false lookup_x = "3")
            "env lookup x" ;
  let lookup_y = Env.lookup env4 "y" in
  unit_test (Env.value_to_string ~printenvp:false lookup_y = "true")
            "env lookup y" ;
  try
    let _ = Env.lookup env4 "m" in
    print_string "env lookup non-existent FAILED\n"
  with
  | _ -> print_string "env lookup non_existent passed\n" ;;

(* TESTS FOR SUBSITUTION AND DYNAMIC EVALUATIONAL MODELS *)

let eval_s_tests () =
  let eval_s' exp = eval_s (str_to_exp exp) (Env.empty ()) in
  unit_test (eval_s' "2;;" = Env.Val (Num 2)) "eval_s number" ;
  unit_test (eval_s' "2 + 2;;" = Env.Val (Num 4))
            "eval_s plus" ;
  unit_test (eval_s' "let x = 2 in x - x;;" =
             Env.Val (Num 0)) "eval_s minus w sub" ;
  unit_test (eval_s' "let x = 2 in ~-(x);;" =
             Env.Val (Num (-2))) "eval_s unop w sub" ;
  unit_test (eval_s' "let x = 2 in if x = 2 then 10 else 2;;"
             = Env.Val (Num 10)) "eval_s conditional w let" ;
  unit_test (eval_s' "let x = 5 in let y = 5 in if x = y then true else false;;"
             = Env.Val (Bool true)) "eval_s double let w cond" ;
  unit_test (eval_s' "let x = 3 in 3 + 3 - 2 * x;;" = Env.Val (Num 0))
            "eval_s let w multiple binops" ;
  unit_test (eval_s' "let x = 5 in let x = 7 in x;;" =
             Env.Val (Num 7)) "eval_s reassignemnt of var" ;
  unit_test (eval_s' "let y = 7 in let y = 8 in y + let x = 5 in x + 5;;" =
             Env.Val (Num 18)) "eval_s reassignment with two funs" ;
  unit_test (eval_s' "let x = 10 in if x < 12 then if true then
            true else false else false;;" = Env.Val (Bool true))
            "eval_s lessthan w double cond" ;
  try
    let _ = eval_s' "let f = 5 in f 4;;" in
    print_string "eval_s forbiddens 1 FAILED\n" ;
    let _ = eval_s' "x;;" in
    print_string "eval_s forbiddens 2 FAILED\n" ;
    let _ = eval_s' "true + 2;;" in
    print_string "eval_s forbiddens 3 FAILED\n" ;
  with
  | _ -> print_string "eval_s forbiddens passed\n" ;;


let eval_d' exp = eval_d (str_to_exp exp) (Env.empty ()) ;;

let eval_d_tests () =
  unit_test (eval_d' "let n = true in if n then true else false ;;" =
             Env.Val (Bool true)) "eval_d cond" ;
  unit_test (eval_d' "let f = fun n -> n + n in f 3 ;;" = Env.Val (Num 6))
            "eval_d simple let" ;
  unit_test (eval_d' "let f = fun m -> m + 4 in let n = fun m -> 2 + m
            in (f 2) + (n 3) ;;" = Env.Val (Num 11))
            "eval_d double let w binop" ;
  unit_test (eval_d' "let rec f = fun n -> if n = 1 then true else
            f (n - 1) in f 100 ;;" = Env.Val (Bool true))
            "eval_d recursion countdown" ;
  unit_test (eval_d' "let rec f = fun n -> if n = 1 then 10 else f (~-n)
            in f (~-1) ;;" = Env.Val (Num 10)) "eval_d simple rec" ;
  unit_test (eval_d' "let f = fun m -> m + 2 in if f 3 < 6 then f 2
            else f 3 ;;" = Env.Val (Num 4)) "eval_d applying let" ;
  unit_test (eval_d' "if 3 < 4 then fun n -> n + n else fun m -> m * m ;;" =
             Env.Val (Fun("n", Binop(Plus, Var "n", Var "n"))))
            "eval_d eval to fun" ;
  unit_test (eval_d' "let m = 4 in let n = 5 in if (2 * m) < n then
            n * n else 1000 ;;" = Env.Val (Num 1000))
            "eval_d double let w binop and cond" ;
  unit_test (eval_d' "let test = 5 in let f = fun n -> n * n in if 3 < 4 then
            f test else f 4 ;;" = Env.Val (Num 25)) "eval_d cond w fun" ;
  unit_test (eval_d' "let f = fun n -> n * n in if (f 3) = (f 4) then
            false else 2 = 2;;" = Env.Val (Bool true))
            "eval_d fun app to bool" ;;

(* EXTENSION TESTS: FLOATS, STRINGS, CURRIED FUNCTION, TRY / WITH *)

let strings_tests () =
  unit_test (eval_d' "\"hello\";;" = Env.Val (String "hello")) "string simple" ;
  unit_test (eval_d' "\"hello\" ^ \"hello\";;" = Env.Val (String "hellohello"))
            "string concat" ;
  unit_test (eval_d' "let x = \"hello\" in x ^ x;;" =
             Env.Val (String "hellohello")) "string concat w sub" ;
  unit_test (eval_d' "let f = fun m -> m ^ m in f \"fun\";;" =
             Env.Val (String "funfun")) "string concat in fun" ;
  unit_test (eval_d' "\"12345\";;" = Env.Val (String "12345"))
            "string of nums" ;
  unit_test (eval_d' "\"if\" ^ \"else\" ^ \"then\";;" =
             Env.Val (String "ifelsethen")) "string w keywords" ;
  unit_test (eval_d' "# \"hey\";;" = Env.Val (Num 3)) "string length simple" ;
  unit_test (eval_d' "if (# \"hello\") = 5 then true else false;;" =
             Env.Val (Bool true)) "string length w if" ;
  unit_test (eval_d' "\"hey\" = \"hey\";;" = Env.Val (Bool true))
            "string equality" ;
  unit_test (eval_d' "\"hey\" = \"test\";;" = Env.Val (Bool false))
            "string inequality" ;
  try
    let _ = eval_d' "\"test\" ^ 4;;" in
    print_string "string forbiddens 1 FAILED\n" ;
    let _ = eval_d' "\"test\" + \"test\";;" in
    print_string "string forbiddens 2 FAILED\n" ;
    let _ = eval_d' "let f = fun m -> m -. m in f \"test\";;" in
    print_string "string forbiddens 3 FAILED\n" ;
  with
  | _ -> print_string "string forbiddens passed\n" ;;

let floats_tests () =
  unit_test (eval_d' "3.2;;" = Env.Val (Float 3.2)) "float simple" ;
  unit_test (eval_d' "1.0 + 2.0;;" = Env.Val (Float 3.0)) "float add" ;
  unit_test (eval_d' "let m = 1.0 in let n = 2.0 in m + n;;" =
             Env.Val (Float 3.0)) "float add w sub" ;
  unit_test (eval_d' "let rec f = fun n -> if n = 1.0 then true
             else f (n - 1.0) in f 100.0;;" = Env.Val (Bool true))
            "float recursion countdown" ;
  unit_test (eval_d' "let f = fun m -> m - m in f 100.0;;" =
             Env.Val (Float 0.0)) "flaot subtraction w fun" ;
  unit_test (eval_d' "3.141 < 3.142;;" = Env.Val (Bool true)) "float lessthan" ;
  unit_test (eval_d' "0.000000 = 0.0;;" = Env.Val (Bool true))
    "float equality w diff form" ;
  try
    let _ = eval_d' "3.0 + 2;;" in
    print_string "float forbiddens 1 FAILED\n" ;
    let _ = eval_d' "let f = fun m -> m ^ m in f 4.0;;" in
    print_string "float forbiddens 2 FAILED\n" ;
  with
  | _ -> print_string "float forbiddens passed\n" ;;

let curried_tests () =
  unit_test (eval_d' "let f x = x + 3 in f 3;;" = Env.Val (Num 6))
            "curried simple" ;
  unit_test (eval_d' "let f x = x * 2.0 = 3.0 in f 1.5;;" =
             Env.Val (Bool true)) "curried bool" ;
  unit_test (eval_d' "let f x = x * x in f 3;;" = Env.Val (Num 9))
            "curried var use twice" ;
  unit_test (eval_d' "let y = 2 in let f x = x + 2 in f y;;" = Env.Val (Num 4))
            "curried reusage of var" ;
  unit_test (eval_d' "let rec f x = if x = 1 then true else f (x - 1)
            in f 100 ;;" = Env.Val (Bool true)) "curried rec countdown" ;
  unit_test (eval_d' "let rec f x = if (# x) = 6 then true else f (x ^ x)
            in f (\"hey\");;" = Env.Val (Bool true)) "curried rec w string op" ;
  unit_test (eval_d' "let rec f x = if x = 2.0 then 100.0 else f (x + 1.0)
            in f 0.0;;" = Env.Val (Float 100.0)) "curried rec w float" ;;

let try_with_tests () =
  unit_test (eval_d' "try 2.0 + 3 with true;;" = Env.Val (Bool true))
            "try with float vs int" ;
  unit_test (eval_d' "try let f x = x - 2 in f true with \"test\";;" =
             Env.Val (String "test")) "try with curried" ;
  unit_test (eval_d' "try 2 + 2 with 4.0 + 2;;" = Env.Val (Num 4))
            "try with w broken 2nd expr" ;
  unit_test (eval_d' "try true with false;;" = Env.Val (Bool true))
            "try with simple" ;
  try
    let _ = eval_d' "try 2.0 + 3 with 4 ^ 4;;" in
    print_string "try with forbidden FAILED\n"
  with
  | _ -> print_string "try with forbidden passed\n" ;;

(* building and running this file will run all the tests above and print
   the results in your terminal *)

free_vars_tests () ;
new_varname_tests () ;
subst_tests () ;
exp_to_concrete_string_tests () ;
exp_to_abstract_string_tests () ;
env_tests () ;
eval_s_tests () ;
eval_d_tests () ;
strings_tests () ;
floats_tests () ;
curried_tests () ;
try_with_tests ()
