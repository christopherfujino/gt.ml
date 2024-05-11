open OUnit2

let make_test program expectation =
  let expr = Gt.Common.parse program in
  let open Gt.Gt_stdlib in
  fun _ ->
    let result = Gt.Interpreter.interpret { identifiers } expr in
    expectation result

let suite =
  let tuples =
    [
      ("number literal", "1", fun r -> assert_equal r (`Number 1.));
      ( "string literal",
        "\"Hello, world!\"",
        fun r -> assert_equal r (`String "Hello, world!") );
      ( "can read HEAD commit",
        "HEAD()",
        function `Commit _ -> () | _ -> assert_failure "Expected a commit" );
    ]
  in
  let tuple_to_test tup =
    let name, program, expectation = tup in
    name >:: make_test program expectation
  in
  "ExampleTestList" >::: List.map tuple_to_test tuples

let () = run_test_tt_main suite
