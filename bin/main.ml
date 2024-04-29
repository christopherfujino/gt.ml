open Gt
open Common

let () =
  let program_opt = try Some (read_line ()) with End_of_file -> None in
  match program_opt with
  | Some program ->
      let expr = parse program in
      let open Gt_stdlib in
      Interpreter.interpret { identifiers } expr
      |> Runtime.to_string |> print_endline
  | None -> ()
