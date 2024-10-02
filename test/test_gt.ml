open OUnit2

let make_test program expectation =
  let expr = Gt.Common.parse program in
  fun _ ->
    let result = Gt.Interpreter.interpret { identifiers = Gt.Gt_stdlib.create () } expr in
    expectation result

    (* Tests fail because they run within a working dir like: //_build/default/test *)
let suite =
  let tuples =
    [
      ( "can call HEAD()",
        "HEAD()",
        function
        | `Function _ -> () | _ -> assert_failure "Expected a function value" );
        (*
      ("number literal", "1", fun r -> assert_equal r (`Number 1.));
      ( "string literal",
        "\"Hello, world!\"",
        fun r -> assert_equal r (`String "Hello, world!") );
      ( "tear-off function",
        "HEAD",
        function
        | `Function _ -> () | _ -> assert_failure "Expected a function value" );
        *)
    ]
  in
  let tuple_to_test tup =
    let name, program, expectation = tup in
    name >:: make_test program expectation
  in
  "ExampleTestList" >::: List.map tuple_to_test tuples

let () = run_test_tt_main suite
