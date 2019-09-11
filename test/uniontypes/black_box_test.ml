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
let test_fixture = "black_box" >:::
[
  "foo" >:: ( fun () ->
              assert_equal 3 4;
  );
]

(* Test Runner; ~verbose:true gives info on succ tests *)
let _ = run_test_tt test_fixture
