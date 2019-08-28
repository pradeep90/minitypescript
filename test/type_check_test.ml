open OUnit
open Type_check
open Syntax

let assert_type_error fn error_msg =
  try
    fn();
    failwith "expected type error"
  with
  | Type_error msg -> if msg = error_msg then ()
                      else failwith ("wrong type error; expected "
                                     ^ error_msg ^ " but got " ^ msg)
;;

(* Test Fixture *)
let test_fixture = "type_check" >:::
[
  "occurs" >:: ( fun () ->
    assert_equal true (occurs "boyz" [("yo",4); ("boyz",3)]);
    assert_equal false (occurs "baz" [("yo",4); ("boyz",3)]);
  );

  "check_labels" >:: ( fun () ->
    assert_equal () (check_labels ["yo"; "boyz"]);
    assert_type_error (fun _ -> check_labels ["yo"; "yo"]) "label yo occurs more than once"
  );

  "var" >:: ( fun () ->
    assert_equal (TBool) (type_of [("x", TInt); ("y", TBool)] (Var "y"));
    assert_type_error (fun _ -> type_of [] (Var "x")) "unknown variable x";
  );

  "int" >:: ( fun () ->
    assert_equal (TInt) (type_of [] (Int 7));
  );

  "arithmetic" >:: ( fun () ->
    assert_equal (TInt) (type_of [("x", TInt); ("y", TBool)] (Plus (Var "x", Int 7)));
    assert_type_error (fun _ -> type_of [("x", TInt); ("y", TBool)] (Minus (Var "x", Var "y"))) "incompatible types";
  );

  "subtype_equals" >:: ( fun () ->
    assert_equal true (subtype TBool TBool);
    assert_equal false (subtype TBool TInt);
  );

  "subtype_records" >:: ( fun () ->
    assert_equal true (subtype (TRecord [("a", TInt)]) (TRecord []));
    assert_equal false (subtype (TRecord []) (TRecord [("a", TInt)]));
    assert_equal false (subtype (TRecord [("b", TInt)]) (TRecord [("a", TInt)]));
    assert_equal true (subtype (TRecord [("a", TRecord [("a", TInt)])]) (TRecord [("a", TRecord [])]));
    assert_equal false (subtype (TRecord [("a", TRecord [])]) (TRecord [("a", TRecord [("a", TInt)])]));
  );

  "subtype_arrows" >:: ( fun () ->
    assert_equal true (subtype (TArrow (TBool, TInt)) (TArrow (TBool, TInt)));
    assert_equal true (subtype (TArrow (TRecord [], TRecord [("a", TInt)])) (TArrow (TRecord [("a", TInt)], TRecord [])));
    assert_equal false (subtype (TArrow (TRecord [("a", TInt)], TRecord [("a", TInt)])) (TArrow (TRecord [], TRecord [])));
  );

  "if" >:: ( fun () ->
    assert_equal (TRecord [("a", TInt)]) (type_of [] (If (Bool true, Record [("a", Int 8)], Record [("a", Int 7); ("b", Bool false)])));
    assert_equal (TRecord [("a", TInt)]) (type_of [] (If (Bool true, Record [("a", Int 7); ("b", Bool false)], Record [("a", Int 8)])));
    assert_type_error (fun _ -> type_of [] (If (Bool true, Record [("a", Int 7)], Record [("b", Bool false)]))) "incompatible types in conditional"
  );

  "fun" >:: ( fun () ->
    assert_equal (TArrow (TInt, TInt)) (type_of [] (Fun ("f", "x", TInt, TInt, Var "x")));
    assert_type_error (fun _ -> type_of [] (Fun ("f", "x", TInt, TBool, Var "x"))) "incompatible types";
    assert_equal (TArrow (TInt, TInt)) (type_of [] (Fun ("f", "x", TInt, TInt, App (Var "f", Var "x"))));
    assert_type_error (fun _ -> type_of [] (Fun ("f", "x", TInt, TInt, App (Var "f", Bool true)))) "incompatible types";
  );

  "let" >:: ( fun () ->
    assert_equal TInt (type_of [("y", TInt)] (Let ("x", Var "y", Var "x")));
  );

  "app" >:: ( fun () ->
    assert_equal TInt (type_of [] (App (Fun ("f", "x", TInt, TInt, Var "x"), Int 8)));
    assert_type_error (fun _ -> type_of [] (App (Fun ("f", "x", TInt, TInt, Var "x"), Bool true))) "incompatible types";
    assert_type_error (fun _ -> type_of [] (App (Int 7, Bool true))) "function expected";
  );
]

(* Test Runner; ~verbose:true gives info on succ tests *)
let _ = run_test_tt test_fixture
