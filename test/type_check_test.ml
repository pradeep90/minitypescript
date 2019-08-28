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
  );

  "var" >:: ( fun () ->
    assert_equal (TBool) (type_of [("x", TInt); ("y", TBool)] (Var "y"));
  );
]

(* Test Runner; ~verbose:true gives info on succ tests *)
let _ = run_test_tt test_fixture
