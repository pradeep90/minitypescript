open OUnit
open Type_check
open Syntax


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
]

(* Test Runner; ~verbose:true gives info on succ tests *)
let _ = run_test_tt test_fixture
