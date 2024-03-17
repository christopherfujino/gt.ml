type value =
  [ `Commit of string
  | `Function of Ast.t list -> value
  | `String of string
  | `Number of float ]
(** Runtime values. *)

type state = { identifiers : (string, value) Hashtbl.t }

let value_to_string (v : value) =
  match v with
  | `Commit rev -> rev
  | `Function _ -> "function"
  | `String s -> s
  | `Number n -> Float.to_string n

let rec interpret state' (e : Ast.t) : value =
  match e with
  | `Number n -> `Number n
  | `String s -> `String s
  | `IdentifierRef id -> Hashtbl.find state'.identifiers id
  | `Invocation (id, args) -> (
      let fun' = Hashtbl.find state'.identifiers id in
      match fun' with
      | `Function cb -> cb args
      | `Commit _ -> raise Main.YoloDawg
      | `String _ -> raise Main.YoloDawg
      | `Number _ -> raise Main.YoloDawg)
  | `MethodInvocation (target, _, _) ->
      let _ = interpret state' target in
      raise (Main.Todo "need to implement methods on types")
