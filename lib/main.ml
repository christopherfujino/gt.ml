let parse input = (Parser.program Lexer.read) (Lexing.from_string input)

let rec printer = function
  | Ast.Number v -> (
      match v with
      | v' when Float.is_integer v' -> string_of_int (int_of_float v')
      | v' -> string_of_float v')
  | Ast.String v -> "\"" ^ v ^ "\""
  | Ast.IdentifierRef i -> i
  | Ast.Invocation (i, e) -> Printf.sprintf "%s(%s)" i (printer e)

type state = {
  foo : unit
};;

let rec interpret e state' =
  match e with
  | Ast.Number _ -> print_endline (printer e)
  | Ast.String _ -> print_endline (printer e)
  | Ast.IdentifierRef _ -> state'.foo
  | Ast.Invocation (_, _) -> state'.foo

let rec interpret_program es state' =
  match es with
  | [] -> ()
  | h :: t ->
      interpret h state';
      interpret_program t state'
