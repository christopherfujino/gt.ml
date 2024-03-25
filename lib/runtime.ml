(** Runtime values. *)

type commit_rec = { hash : string; date : int64 * Git.User.tz_offset option }

type t =
  | Commit of commit_rec
  | Function of (Ast.t list -> t)
  | String of string
  | Number of float

let to_string (v : t) =
  match v with
  (* TODO *)
  | Commit c -> c.hash
  | Function _ -> "function"
  | String s -> "\"" ^ s ^ "\""
  | Number n -> Float.to_string n
