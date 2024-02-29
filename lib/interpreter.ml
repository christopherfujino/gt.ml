type value =
  | Commit of string
  | Function of (Ast.expr list -> value)
  | StringValue of string

type state = { identifiers : (string, value) Hashtbl.t }

let rec expr_to_string = function
  | Ast.Number v -> (
      match v with
      | v' when Float.is_integer v' -> string_of_int (int_of_float v')
      | v' -> string_of_float v')
  | Ast.String v -> "\"" ^ v ^ "\""
  | Ast.IdentifierRef i -> i
  | Ast.Invocation (i, es) ->
      let cb acc cur = acc ^ expr_to_string cur in
      (* Is this the right direction? *)
      let concatenated_es = List.fold_left cb "" es in
      Printf.sprintf "%s(%s)" i concatenated_es

let value_to_string v =
  match v with
  | Commit rev -> rev
  | Function _ -> "function"
  | StringValue s -> s

let interpret state' e =
  match e with
  | Ast.Number _ -> print_endline (expr_to_string e)
  | Ast.String _ -> print_endline (expr_to_string e)
  | Ast.IdentifierRef id ->
      print_endline (value_to_string (Hashtbl.find state'.identifiers id))
  | Ast.Invocation (id, args) -> (
      let fun' = Hashtbl.find state'.identifiers id in
      match fun' with
      | Function cb -> cb args |> value_to_string |> print_endline
      | Commit rev -> print_endline rev
      | StringValue s -> print_endline s)

let rec interpret_program es state' =
  match es with
  | [] -> ()
  | h :: t ->
      interpret h state';
      interpret_program t state'
