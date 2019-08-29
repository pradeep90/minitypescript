open OUnit
open Type_check
open Syntax

let assert_type_error fn error_msg =
  try
    fn();
    failwith "expected type error"
  with
  | Type_error msg -> if msg = error_msg then ()
                      else failwith ("wrong type error; expected `"
                                     ^ error_msg ^ "` but got `" ^ msg ^ "`")
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
    assert_type_error (fun _ -> type_of [("x", TInt); ("y", TBool)] (Minus (Var "x", Var "y"))) "incompatible types; bool is not a subtype of int";
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
    assert_type_error (fun _ -> type_of [] (Fun ("f", "x", TInt, TBool, Var "x"))) "incompatible types; int is not a subtype of bool";
    assert_equal (TArrow (TInt, TInt)) (type_of [] (Fun ("f", "x", TInt, TInt, App (Var "f", Var "x"))));
    assert_type_error (fun _ -> type_of [] (Fun ("f", "x", TInt, TInt, App (Var "f", Bool true)))) "incompatible types; bool is not a subtype of int";
            );

  "fun_aliases" >:: ( fun () ->
    assert_equal (TArrow (TInt, TInt)) (type_of [("Foo", TInt)] (Fun ("f", "x", TAlias "Foo", TAlias "Foo", App (Var "f", Var "x"))));
            );

  "fun_type_params" >:: ( fun () ->
    assert_equal (TArrow (TParam "Foo", TParam "Foo")) (type_of [] (Fun ("f", "x", TAlias "Foo", TAlias "Foo", Var "x")));
            );

  "let" >:: ( fun () ->
    assert_equal TInt (type_of [("y", TInt)] (Let ("x", Var "y", Var "x")));
  );

  "app" >:: ( fun () ->
    assert_equal TInt (type_of [] (App (Fun ("f", "x", TInt, TInt, Var "x"), Int 8)));
    assert_type_error (fun _ -> type_of [] (App (Fun ("f", "x", TInt, TInt, Var "x"), Bool true))) "incompatible types; bool is not a subtype of int";
    assert_type_error (fun _ -> type_of [] (App (Int 7, Bool true))) "function expected";
  );

  "app_parameterized_function" >:: ( fun () ->
    assert_equal TBool (type_of [] (App (Fun ("f", "x", TParam "Foo", TParam "Foo", Var "x"), Bool true)));
  );

  "record" >:: ( fun () ->
    assert_type_error (fun _ -> type_of [] (Record [("a", Int 7); ("a", Bool true)])) "label a occurs more than once";
    assert_equal (TRecord [("a", TInt); ("b", TRecord [("a", TBool)])]) (type_of [] (Record [("a", Int 7); ("b", Record [("a", Bool true)])]));
  );

  "project" >:: ( fun () ->
    assert_equal TInt (type_of [] (Project (Record [("a", Int 7); ("b", Record [("c", Bool true)])], "a")));
    assert_type_error (fun _ -> type_of [] (Project (Record [("a", Int 7); ("b", Record [("c", Bool true)])], "c"))) "no such field c";
    assert_type_error (fun _ -> type_of [] (Project (Int 7, "c"))) "record expected";
  );

  "substitute_aliases_maybe" >:: ( fun () ->
    assert_equal TInt (substitute_aliases_maybe [] TInt);
    assert_equal TBool (substitute_aliases_maybe [] TBool);
    assert_equal TInt (substitute_aliases_maybe [("bar", TBool); ("foo", TInt)] (TAlias "foo"));
    assert_equal (TArrow (TInt, TBool)) (substitute_aliases_maybe [("bar", TBool); ("foo", TInt)] (TArrow (TAlias "foo", TAlias "bar")));
    assert_equal (TRecord [("a", TInt); ("b", TBool)]) (substitute_aliases_maybe [("bar", TBool); ("foo", TInt)] (TRecord [("a", TAlias "foo"); ("b", TAlias "bar")]));

    assert_equal (TRecord [("a", TAlias "foo")]) (substitute_aliases_maybe [] (TRecord [("a", TAlias "foo")]));
  );

  "has_no_aliases" >:: ( fun () ->
    assert_equal true (has_no_aliases TInt);
    assert_equal true (has_no_aliases TBool);
    assert_equal false (has_no_aliases (TAlias "foo"));
    assert_equal false (has_no_aliases (TArrow (TAlias "foo", TInt)));
    assert_equal false (has_no_aliases (TArrow (TInt, TAlias "foo")));
    assert_equal true (has_no_aliases (TArrow (TInt, TBool)));
    assert_equal false (has_no_aliases (TRecord [("a", TAlias "foo"); ("b", TInt)]));
    assert_equal true (has_no_aliases (TRecord [("a", TInt); ("b", TBool)]));
 );

  "make_alias_param" >:: ( fun () ->
    assert_equal TInt (make_alias_param TInt);
    assert_equal TBool (make_alias_param TBool);
    assert_equal (TParam "foo") (make_alias_param (TAlias "foo"));
    assert_equal (TArrow (TParam "foo", TParam "bar")) (make_alias_param (TArrow (TAlias "foo", TAlias "bar")));
    assert_equal (TRecord [("a", TParam "foo"); ("b", TParam "bar")]) (make_alias_param (TRecord [("a", TAlias "foo"); ("b", TAlias "bar")]));
  );

  "param_constraints" >:: ( fun () ->
    assert_equal [] (param_constraints TInt TInt);
    assert_type_error (fun _ -> param_constraints TInt (TParam "bar")) "subtype bar cannot have parameters when supertype is int";

    assert_equal [("foo", TInt)] (param_constraints (TParam "foo") TInt);
    assert_equal [("foo", TBool)] (param_constraints (TParam "foo") TBool);
    assert_equal [] (param_constraints (TParam "foo") (TParam "foo"));
    assert_type_error (fun _ -> param_constraints (TParam "foo") (TParam "bar")) "cannot unify different type parameters foo and bar";
    assert_equal [("foo", TArrow (TInt, TBool))] (param_constraints (TParam "foo") (TArrow (TInt, TBool)));
    assert_equal [("foo", TRecord [("a", TInt)])] (param_constraints (TParam "foo") (TRecord [("a", TInt)]));
    assert_equal [("foo", TInt)] (param_constraints (TRecord [("a", TParam "foo")]) (TRecord [("a", TInt); ("b", TBool)]));
    assert_equal [("foo", TInt); ("foo", TBool)] (param_constraints (TRecord [("a", TParam "foo"); ("b", TParam "foo")]) (TRecord [("a", TInt); ("b", TBool)]));
    assert_equal [("bar", TBool)] (param_constraints (TArrow (TInt, TParam "bar")) (TArrow (TInt, TBool)));

    assert_type_error (fun _ -> param_constraints (TRecord [("a", TParam "foo"); ("b", TParam "foo")]) TInt) "cannot unify {a : foo, b : foo} and int";
  );

  "unify_constraints" >:: ( fun () ->
    assert_equal [] (unify_constraints [] []);
    assert_equal [("bar", TBool); ("foo", TInt); ("baz", TInt)] (unify_constraints [("baz", TInt)] [("foo", TInt); ("bar", TBool)]);
    assert_type_error (fun _ -> unify_constraints [] [("foo", TInt); ("foo", TBool)]) "cannot unify the constraints that type parameter foo : bool and foo : int";

    assert_equal [("bar", TBool); ("foo", TRecord [("a", TInt)]); ("foo", TRecord [("a", TInt)])] (unify_constraints [("foo", TRecord [("a", TInt)])] [("foo", TRecord [("a", TInt); ("b", TBool)]); ("bar", TBool)]);
    assert_equal [("bar", TBool); ("foo", TRecord [("a", TInt)]); ("foo", TRecord [("a", TInt); ("b", TBool)])] (unify_constraints [("foo", TRecord [("a", TInt); ("b", TBool)])] [("foo", TRecord [("a", TInt)]); ("bar", TBool)]);
  );
]

(* Test Runner; ~verbose:true gives info on succ tests *)
let _ = run_test_tt test_fixture
