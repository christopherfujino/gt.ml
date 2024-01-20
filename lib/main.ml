let parse input = (Parser.program Lexer.read) (Lexing.from_string input)

exception YoloDawg

let rec expr_to_string = function
  | Ast.Number v -> (
      match v with
      | v' when Float.is_integer v' -> string_of_int (int_of_float v')
      | v' -> string_of_float v')
  | Ast.String v -> "\"" ^ v ^ "\""
  | Ast.IdentifierRef i -> i
  | Ast.Invocation (i, e) -> Printf.sprintf "%s(%s)" i (expr_to_string e)

type value =
  | Commit of string
  | Function of (Ast.expr -> value)
  | StringValue of string

let value_to_string = function
  | Commit rev -> rev
  | Function _ -> "function"
  | StringValue s -> s

type state = { foo : unit; identifiers : (string, value) Hashtbl.t }

let interpret state' e =
  match e with
  | Ast.Number _ -> print_endline (expr_to_string e)
  | Ast.String _ -> print_endline (expr_to_string e)
  | Ast.IdentifierRef id ->
      print_endline (value_to_string (Hashtbl.find state'.identifiers id))
  | Ast.Invocation (id, arg) -> (
      let fun' = Hashtbl.find state'.identifiers id in
      match fun' with
      | Function cb -> cb arg |> value_to_string |> print_endline
      | _ ->
          print_string "foo: ";
          fun' |> value_to_string |> print_endline;
          raise YoloDawg)

let rec interpret_program es state' =
  match es with
  | [] -> ()
  | h :: t ->
      interpret h state';
      interpret_program t state'
