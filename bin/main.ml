open Gt
open Common

let m =
  (module Git_foo.Make (struct
    let root = Sys.getcwd ()
  end) : Git_foo.Git_type)

let rec interpret_line () =
  let program_opt = try Some (read_line ()) with End_of_file -> None in
  match program_opt with
  | Some program ->
      let expr = parse program in
      let stdlib = Gt_stdlib.create m in
      Interpreter.interpret { identifiers = stdlib } expr
      |> Runtime.to_string |> print_endline;
      (interpret_line [@tailcall]) ()
  | None -> ()

let () = interpret_line ()
