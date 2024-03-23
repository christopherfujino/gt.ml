(** Runtime values. *)
type value =
  | Commit of string
  | Function of (Ast.t list -> value)
  | String of string
  | Number of float

let fields v =
  let open Main in
  match v with
  | Commit _ -> raise Todo
  | Function _ -> raise Todo
  | String _ -> raise Todo
  | Number _ -> raise Todo

type state = { identifiers : (string, value) Hashtbl.t }

let value_to_string (v : value) =
  match v with
  | Commit rev -> rev
  | Function _ -> "function"
  | String s -> s
  | Number n -> Float.to_string n

let rec interpret state' e =
  match e with
  | Ast.Number n -> Number n
  | Ast.String s -> String s
  | Ast.IdentifierRef id -> Hashtbl.find state'.identifiers id
  | Ast.Invocation (id, args) -> (
      let fun' = Hashtbl.find state'.identifiers id in
      let open Main in
      match fun' with
      | Function cb -> cb args
      | Commit _ -> raise Todo
      | String _ -> raise Todo
      | Number _ -> raise Todo)
  | Ast.MethodInvocation (ast_target, _, _) ->
      let _ = interpret state' ast_target in
      raise Main.Todo
