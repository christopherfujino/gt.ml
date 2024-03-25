open Main

let fields v =
  let open Runtime in
  match v with
  | Commit _ -> raise Todo
  | Function _ -> raise Todo
  | String _ -> raise Todo
  | Number _ -> raise Todo

type state = { identifiers : (string, Runtime.t) Hashtbl.t }

let rec interpret state' e =
  match e with
  | Ast.Number n -> Runtime.Number n
  | Ast.String s -> Runtime.String s
  | Ast.IdentifierRef id -> Hashtbl.find state'.identifiers id
  | Ast.Invocation (id, args) -> (
      let fun' = Hashtbl.find state'.identifiers id in
      match fun' with
      | Runtime.Function cb -> cb args
      | Runtime.Commit _ -> raise Todo
      | Runtime.String _ -> raise Todo
      | Runtime.Number _ -> raise Todo)
  | Ast.MethodInvocation (ast_target, field_name, args) -> (
      let target = interpret state' ast_target in
      match target with
      | Commit c ->
          let cb = Commit_runtime.stdlib c field_name in
          cb args
      | Function _ -> raise Todo
      | String _ -> raise Todo
      | Number _ -> raise Todo)
