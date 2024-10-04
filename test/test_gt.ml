open OUnit2

let git_foo =
  let root = Sys.getenv "PWD" in
  (module Gt.Git_foo.Make (struct
    let root = root
  end) : Gt.Git_foo.Git_type)

let make_test program expectation =
  let expr = Gt.Common.parse program in
  Out_channel.flush Out_channel.stdout;
  print_endline "making a test!";
  fun _ ->
    let interpreter_state : Gt.Interpreter.state =
      { identifiers = Gt.Gt_stdlib.create git_foo }
    in
    print_endline "about to interpret an expression";
    let result = Gt.Interpreter.interpret interpreter_state expr in
    print_endline "interpreted an expression";
    expectation result

let () = print_endline "starting test suite"

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
