(** Runtime values. *)

module Commit = struct
  module User = struct
    type t = { name : string; date : int64 (* TODO include time zone offset *) }

    let of_git_user (user' : Git.User.t) =
      let unix_time, _ = user'.date in
      { name = user'.name; date = unix_time }
  end

  type t = { committer : User.t; revision : string }
end

type t =
  | Commit of Commit.t
  | Function of (Ast.t list -> t)
  | String of string
  | Number of float

let to_string (v : t) : string =
  match v with
  (* TODO *)
  | Commit c ->
      Printf.sprintf "%s at %s" c.committer.name
        (Int64.to_string c.committer.date)
  | Function _ -> "function"
  | String s -> "\"" ^ s ^ "\""
  | Number n -> Float.to_string n
