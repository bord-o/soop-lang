open OUnit2


let hellobf = ">++++++++[<+++++++++>-]<.>++++[<+++++++>-]<+.+++++++..+++.>>++++++[<+++++++>-]<+
+.------------.>++++++[<+++++++++>-]<+.<.+++.------.--------.>>>++++[<++++++++>-
]<+."

let () = print_endline hellobf

let test_sym _ =
    assert_equal 1 1

let test_suite =
  "caml fk tests" >::: [
    "t1" >:: test_sym;
  ]

let () =
  run_test_tt_main test_suite
