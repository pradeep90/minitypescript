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

  "subtype_forall" >:: ( fun () ->
    assert_equal true (subtype (TForAll ("A", KStar, TInt)) (TForAll ("A", KStar, TInt)));
    let forall_arrow = TForAll ("A", KStar, TArrow (TParam "A", TParam "A"))
    and forall_arrow2 = TForAll ("B", KStar, TArrow (TParam "B", TParam "B"))
    and forall_arrow_same_name = TForAll ("A", KStar, TArrow (TParam "A", TParam "A"))
    in
    assert_equal true (subtype forall_arrow forall_arrow);
    assert_equal true (subtype forall_arrow forall_arrow_same_name);
    assert_equal true (subtype forall_arrow forall_arrow2);
    assert_equal false (subtype forall_arrow (TForAll ("A", KStar, TInt)));
    assert_equal false (subtype forall_arrow (TForAll ("A", KStar, TArrow (TInt, TParam "A"))));
  );

  "if" >:: ( fun () ->
    assert_equal (TRecord [("a", TInt)]) (type_of [] (If (Bool true, Record [("a", Int 8)], Record [("a", Int 7); ("b", Bool false)])));
    assert_equal (TRecord [("a", TInt)]) (type_of [] (If (Bool true, Record [("a", Int 7); ("b", Bool false)], Record [("a", Int 8)])));
    assert_type_error (fun _ -> type_of [] (If (Bool true, Record [("a", Int 7)], Record [("b", Bool false)]))) "incompatible types in conditional"
  );

  "fun" >:: ( fun () ->
    assert_equal (TArrow (TInt, TInt)) (type_of [] (Fun ("f", "x", TArrow (TInt, TInt), Var "x")));
    assert_type_error (fun _ -> type_of [] (Fun ("f", "x", TArrow (TInt, TBool), Var "x"))) "incompatible types; int is not a subtype of bool";
    assert_equal (TArrow (TInt, TInt)) (type_of [] (Fun ("f", "x", TArrow (TInt, TInt), App (Var "f", Var "x"))));
    assert_type_error (fun _ -> type_of [] (Fun ("f", "x", TArrow (TInt, TInt), App (Var "f", Bool true)))) "incompatible types; bool is not a subtype of int";
            );

  "fun_type_params" >:: ( fun () ->
    assert_equal (TArrow (TParam "Foo", TParam "Foo")) (type_of [] (Fun ("f", "x", TArrow (TParam "Foo", TParam "Foo"), Var "x")));
    assert_type_error (fun _ -> type_of [] (Fun ("f", "x", TArrow (TParam "Foo", TParam "Foo"), Int 3))) "incompatible types; int is not a subtype of Foo";
    assert_type_error (fun _ -> type_of [] (Fun ("f", "x", TArrow (TParam "Foo", TParam "Bar"), Var "x"))) "incompatible types; Foo is not a subtype of Bar";
            );

  "let" >:: ( fun () ->
    assert_equal TInt (type_of [("y", TInt)] (Let ("x", Var "y", Var "x")));
  );

  "app" >:: ( fun () ->
    assert_equal TInt (type_of [] (App (Fun ("f", "x", TArrow (TInt, TInt), Var "x"), Int 8)));
    assert_type_error (fun _ -> type_of [] (App (Fun ("f", "x", TArrow (TInt, TInt), Var "x"), Bool true))) "incompatible types; bool is not a subtype of int";
    assert_type_error (fun _ -> type_of [] (App (Int 7, Bool true))) "expected function but got int";
  );

  "app_parameterized_function" >:: ( fun () ->
    assert_equal (TArrow (TBool, TBool)) (type_of [] (TApp (TFun ("Foo", KStar, Fun ("id", "x", TArrow (TParam "Foo", TParam "Foo"), Var "x")), TBool)));
    assert_equal TBool (type_of [] (App (TApp (TFun ("Foo", KStar, Fun ("id", "x", TArrow (TParam "Foo", TParam "Foo"), Var "x")), TBool), Bool true)));

    let id_fun = TFun ("Foo", KStar, Fun ("id", "x", TArrow (TParam "Foo", TParam "Foo"), Var "x"))
    and id_fun_type = TForAll ("Foo", KStar, TArrow (TParam "Foo", TParam "Foo"))
    in
    assert_equal (id_fun_type) (type_of [] (App (TApp (id_fun, id_fun_type), id_fun)));

    let foo_fun = TFun ("Foo", KStar, Fun ("foo", "x", TArrow (TParam "Foo", TInt), Int 3))
    and foo_type = TForAll ("Foo", KStar, TArrow (TParam "Foo", TInt))
    in
    assert_equal TInt (type_of [] (App (TApp (foo_fun, foo_type), foo_fun)));

    assert_equal TInt (type_of [] (App (App (TApp (TApp (TFun ("Foo", KStar, TFun ("Bar", KStar, Fun ("const", "x", TArrow (TParam "Foo", TArrow (TParam "Bar", TParam "Foo")), Fun ("f", "y", TArrow (TParam "Bar", TParam "Foo"), Var "x")))), TInt), TBool), Int 1), Bool true)));

    assert_equal (TRecord [("a", TInt); ("b", TInt)]) (type_of [] (App (TApp (TFun ("Foo", KStar, Fun ("f", "x", TArrow (TRecord [("a", TParam "Foo"); ("b", TParam "Foo")], TRecord [("a", TParam "Foo"); ("b", TParam "Foo")]), Var "x")), TInt), Record [("a", Int 1); ("b", Int 2)])));

    assert_type_error (fun _ -> type_of [] (App (TApp (TFun ("Foo", KStar, Fun ("f", "x", TArrow (TRecord [("a", TParam "Foo"); ("b", TParam "Foo")], TRecord [("a", TParam "Foo"); ("b", TParam "Foo")]), Var "x")), TInt), Record [("a", Int 1); ("b", Bool true)]))) "incompatible types; {a : int, b : bool} is not a subtype of {a : int, b : int}";

    assert_equal (TRecord [("a", TRecord [("c", TInt)]); ("b", TRecord [("c", TInt)])]) (type_of [] (App (TApp (TFun("Foo", KStar, Fun ("f", "x", TArrow (TRecord [("a", TParam "Foo"); ("b", TParam "Foo")], TRecord [("a", TParam "Foo"); ("b", TParam "Foo")]), Var "x")), TRecord [("c", TInt)]), Record [("a", Record [("c", Int 1)]); ("b", Record [("c", Int 1); ("d", Bool true)])])));
    assert_type_error (fun _ -> type_of [] (App (TApp (TFun("Foo", KStar, Fun ("f", "x", TArrow (TRecord [("a", TParam "Foo"); ("b", TParam "Foo")], TRecord [("a", TParam "Foo"); ("b", TParam "Foo")]), Var "x")), TRecord [("c", TInt); ("d", TBool)]), Record [("a", Record [("c", Int 1)]); ("b", Record [("c", Int 1); ("d", Bool true)])]))) "incompatible types; {a : {c : int}, b : {c : int, d : bool}} is not a subtype of {a : {c : int, d : bool}, b : {c : int, d : bool}}";

    assert_equal TInt (type_of [] (App (TApp (TApp (TFun ("B", KStar, TFun ("A", KStar, Fun ("dummy", "f", TArrow (TArrow (TParam "A", TParam "B"), TInt), Int 3))), TInt), TInt), Fun ("increment", "x", TArrow (TInt, TInt), Plus (Var "x", Int 1)))));
    assert_equal (TArrow (TInt, TInt)) (type_of [] (App (TApp (TApp (TFun ("B", KStar, TFun ("A", KStar, Fun ("dummy", "f", TArrow (TArrow (TParam "A", TParam "B"), TArrow (TParam "A", TParam "B")), Var "f"))), TInt), TInt), Fun ("increment", "x", TArrow (TInt, TInt), Plus (Var "x", Int 1)))));
  );

  "tapp" >:: ( fun () ->
    let id_fun = TFun ("Foo", KStar, Fun ("id", "x", TArrow (TParam "Foo", TParam "Foo"), Var "x"))
    in
    assert_equal (TArrow (TInt, TInt)) (type_of [] (TApp (id_fun, TInt)));
    assert_type_error (fun _ -> type_of [] (TApp (Int 3, TInt))) "expected `forall`, but got int";
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

  "substitute_params_maybe" >:: ( fun () ->
    assert_equal (TForAll ("Bar", KStar, TInt)) (substitute_params_maybe [("Foo", TInt)] (TForAll ("Bar", KStar, TParam "Foo")));
    assert_equal (TForAll ("Foo", KStar, TParam "Foo")) (substitute_params_maybe [("Foo", TInt)] (TForAll ("Foo", KStar, TParam "Foo")));
  );
]

(* Test Runner; ~verbose:true gives info on succ tests *)
let _ = run_test_tt test_fixture
