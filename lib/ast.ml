(** Parser AST node type. *)
type t =
  | Number of float
  | String of string
  | Invocation of string * (t list)
  | IdentifierRef of string

let rec to_string = function
  | Number v -> (
      match v with
      | v' when Float.is_integer v' -> string_of_int (int_of_float v')
      | v' -> string_of_float v')
  | String v -> "\"" ^ v ^ "\""
  | IdentifierRef i -> i
  | Invocation (i, es) ->
      let cb acc cur = acc ^ to_string cur in
      (* Is this the right direction? *)
      let concatenated_es = List.fold_left cb "" es in
      Printf.sprintf "%s(%s)" i concatenated_es
