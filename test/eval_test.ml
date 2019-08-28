open OUnit
open Eval
open Syntax


(* Test Fixture *)
let test_fixture = "eval" >:::
[
  "lookup_value" >:: ( fun () ->
    assert_equal 3 (lookup_value "boyz" [("yo",4); ("boyz",3)]) ~printer:string_of_int;
  );

  "Var" >:: (fun () ->
    assert_equal (Var "boyz") (eval [("yo", Var "boyz")] (Var "yo"))
  );
]

(* Test Runner; ~verbose:true gives info on succ tests *)
let _ = run_test_tt test_fixture
