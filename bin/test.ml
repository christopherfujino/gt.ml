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

let m =
  (module Git_foo.Make (struct
    let root = Sys.getcwd ()
  end) : Git_foo.Git_type)

let () =
  List.iter
    (fun program ->
      let expr = parse program in
      print_endline program;
      Interpreter.interpret { identifiers = (Gt_stdlib.create m) } expr
      |> Runtime.to_string |> print_endline)
    programs
