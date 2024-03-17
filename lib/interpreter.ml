(** Runtime values. *)
type value =
  [ `Commit of string | `Function of Ast.t list -> value | `String of string ]

type state = { identifiers : (string, value) Hashtbl.t }

let value_to_string v =
  match v with
  | `Commit rev -> rev
  | `Function _ -> "function"
  | `String s -> s

let interpret state' e =
  let open Ast in
  match e with
  | Number _ -> print_endline (to_string e)
  | String _ -> print_endline (to_string e)
  | IdentifierRef id ->
      print_endline (value_to_string (Hashtbl.find state'.identifiers id))
  | Invocation (id, args) -> (
      let fun' = Hashtbl.find state'.identifiers id in
      match fun' with
      | `Function cb -> cb args |> value_to_string |> print_endline
      | `Commit rev -> print_endline rev
      | `String s -> print_endline s)
