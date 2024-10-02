open Gt
open Common

let rec interpret_line () =
  let program_opt = try Some (read_line ()) with End_of_file -> None in
  match program_opt with
  | Some program ->
      let expr = parse program in
      Interpreter.interpret { identifiers = Gt_stdlib.create () } expr
      |> Runtime.to_string |> print_endline;
      (interpret_line [@tailcall]) ()
  | None -> ()

let () = interpret_line ()
