open OUnit
open Eval
open Syntax


(* Test Fixture *)
let test_fixture = "eval" >:::
[
  "lookup_value" >:: ( fun () ->
    assert_equal 3 (lookup_value "boyz" [("yo",4); ("boyz",3)]) ~printer:string_of_int;
  );

  "var" >:: (fun () ->
    assert_equal (Bool true) (eval [("yo", Bool true)] (Var "yo"))
  );

  "int" >:: (fun () ->
    assert_equal (Int 7) (eval [] (Int 7))
  );

  "plus" >:: (fun () ->
    assert_equal (Int 15) (eval [("yo", Int 8)] (Plus (Var "yo", Int 7)))
  );

  "minus" >:: (fun () ->
    assert_equal (Int (-1)) (eval [("yo", Int 8)] (Minus (Int 7, Var "yo")))
  );

  "times" >:: (fun () ->
    assert_equal (Int 56) (eval [("yo", Int 8)] (Times (Int 7, Var "yo")))
  );

  "divide" >:: (fun () ->
    assert_equal (Int 1) (eval [("yo", Int 8)] (Divide (Int 9, Var "yo")))
  );

  "bool" >:: (fun () ->
    assert_equal (Bool true) (eval [] (Bool true))
  );

  "equal" >:: (fun () ->
    assert_equal (Bool true) (eval [] (Equal (Int 7, Int 7)));
    assert_equal (Bool false) (eval [] (Equal (Int 7, Int 8)))
  );

  "less" >:: (fun () ->
    assert_equal (Bool true) (eval [] (Less (Int 7, Int 8)));
    assert_equal (Bool false) (eval [] (Less (Int 9, Int 8)))
  );

  "and" >:: (fun () ->
    assert_equal (Bool true) (eval [] (And (Bool true, Bool true)));
    assert_equal (Bool false) (eval [] (And (Bool true, Bool false)))
  );

  "or" >:: (fun () ->
    assert_equal (Bool true) (eval [] (Or (Bool true, Bool false)));
    assert_equal (Bool false) (eval [] (Or (Bool false, Bool false)))
  );

  "not" >:: (fun () ->
    assert_equal (Bool true) (eval [] (Not (Bool false)));
    assert_equal (Bool false) (eval [] (Not (Bool true)))
  );

  "if" >:: (fun () ->
    assert_equal (Int 3) (eval [] (If (Bool true, Int 3, Divide (Int 7, Int 0))));
    assert_equal (Int 4) (eval [] (If (Bool false, Divide (Int 7, Int 0), Int 4)))
  );

  "fun" >:: (fun () ->
    let closure = (eval [("yo", Int 7)] (Fun ("f", "x", TInt, TInt, Var "x")))
    in
    match closure with
    | Closure (env, param, body) ->
       assert_equal "x" param;
       assert_equal (Var "x") body;
       assert_equal (Int 7) (List.assoc "yo" env);
       assert_equal true (closure == (List.assoc "f" env)) ~msg:"function name should point to the closure";
    | _ -> runtime_error "wrong output"
  );

  "closure" >:: (fun () ->
    let rec closure = Closure ([("f", closure)], "x", Var "x")
    in
    assert_equal true (closure == eval [] closure);
  );

  "let" >:: (fun () ->
    assert_equal (Int 7) (eval [] (Let ("x", Int 7, Var "x")));
    assert_equal (Int 15) (eval [] (Let ("x", Int 7, (Let ("y", Int 8, Plus (Var "x", Var "y"))))));
  );

  "app" >:: (fun () ->
    assert_equal (Int 8) (eval [("yo", Int 7)] (App (Fun ("f", "x", TInt, TInt, Var "x"), Int 8)));
    assert_equal (Int 15) (eval [("yo", Int 7)] (App (Fun ("f", "x", TInt, TInt, Plus (Var "x", Var "yo")), Int 8)));
  );

  "app_recursive" >:: (fun () ->
    assert_equal (Int 120) (eval [] (App (Fun ("fact", "x", TInt, TInt, If (Equal (Var "x", Int 1), Int 1, Times (Var "x", App (Var "fact", Minus (Var "x", Int 1))))), Int 5)));
  );

  "record" >:: (fun () ->
    assert_equal (Record [("a", Int 7); ("b", Record [("nested", Bool true)])]) (eval [("yo", Int 7)] (Record [("a", Var "yo"); ("b", Record [("nested", Bool true)])]));
  );

  "project" >:: (fun () ->
    assert_equal (Int 7) (eval [("yo", Int 7)] (Project (Record [("a", Var "yo"); ("b", Record [("a", Bool true)])], "a")));
    assert_equal (Bool true) (eval [("yo", Int 7)] (Project (Project (Record [("a", Var "yo"); ("b", Record [("a", Bool true)])], "b"), "a")));
  );
]

(* Test Runner; ~verbose:true gives info on succ tests *)
let _ = run_test_tt test_fixture
