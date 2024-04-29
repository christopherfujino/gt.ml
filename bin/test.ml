open Gt
open Common

let programs =
  [
    {|1|};
    {|"Hello, world!"|};
    {|HEAD()|};
    {|HEAD().date()|};
    {|HEAD|};
    {|print("Hello, world!")|};
  ]

let () =
  List.iter
    (fun program ->
      let expr = parse program in
      print_endline program;
      let open Gt_stdlib in
      Interpreter.interpret { identifiers } expr |> Runtime.to_string |> print_endline)
    programs
