open Common

let fields v =
  match v with
  | `Commit _ -> raise Todo
  | `Function _ -> raise Todo
  | `String _ -> raise Todo
  | `Number _ -> raise Todo
  | `Date _ -> raise Todo

type state = { identifiers : (string, Runtime.t) Hashtbl.t }

let rec interpret state' e =
  match e with
  | Ast.Number n -> `Number n
  | Ast.String s -> `String s
  | Ast.IdentifierRef id -> Hashtbl.find state'.identifiers id
  | Ast.Invocation (id, args) -> (
      let fun' = Hashtbl.find state'.identifiers id in
      match fun' with
      | `Function cb -> cb args
      | `Commit _ -> raise Todo
      | `String _ -> raise Todo
      | `Number _ -> raise Todo
      | `Date _ -> raise Todo)
  | Ast.MethodInvocation (ast_target, field_name, args) -> (
      let target = interpret state' ast_target in
      match target with
      | `Commit c ->
          let cb = Runtime.Commit.get_method c field_name in
          cb args
      | `Function _ -> raise Todo
      | `String _ -> raise Todo
      | `Number _ -> raise Todo
      | `Date _ -> raise Todo)
