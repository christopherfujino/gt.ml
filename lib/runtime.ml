(** Runtime values. *)

open Common

module Commit = struct
  module User = struct
    type t = {
      name : string;
      date : Unix.tm; (* TODO include time zone offset *)
    }

    let of_git_user (user' : Git.User.t) =
      let unix_time, _ = user'.date in
      let unix_time_fl = Int64.to_float unix_time in
      { name = user'.name; date = Unix.gmtime unix_time_fl }
  end

  type t = { committer : User.t; revision : string }

  let of_store_value store_value =
    let open Git_foo in
    let hash = Store.Value.digest store_value in
    let hash_str = Store.Hash.to_hex hash in
    match store_value with
    | Commit c ->
        let committer = Store.Value.Commit.committer c in
        { committer = User.of_git_user committer; revision = hash_str }
    | Blob _ -> raise Unreachable
    | Tree _ -> raise Unreachable
    | Tag _ -> raise Unreachable

  let get_method receiver func_name =
    match func_name with
    | "revision" -> fun _ -> `String receiver.revision
    | "date" -> fun _ -> `Date receiver.committer.date
    | _ -> raise (YoloDawg func_name)
end

(* This must be polymorphic since there exists a circuluar dependency between
   this and the Commit submodule. *)
type t =
  [ `Commit of Commit.t
  | `Function of Ast.t list -> t
  | `String of string
  | `Number of float
  | `Date of Unix.tm ]

let rec to_string (v : t) : string =
  match v with
  | `Commit c ->
      let date = `Date c.committer.date in
      Printf.sprintf "%s at %s" c.committer.name (to_string date)
  | `Function _ -> "function"
  | `String s -> "\"" ^ s ^ "\""
  | `Number n -> Float.to_string n
  | `Date tm ->
      let month =
        match tm.tm_mon with
        | 0 -> "Jan"
        | 1 -> "Feb"
        | 2 -> "Mar"
        | 3 -> "Apr"
        | 4 -> "May"
        | 5 -> "Jun"
        | 6 -> "Jul"
        | 7 -> "Aug"
        | 8 -> "Sep"
        | 9 -> "Oct"
        | 10 -> "Nov"
        | 11 -> "Dec"
        | _ -> raise Unreachable
      in
      let day = tm.tm_mday in
      let year = tm.tm_year + 1900 in
      Printf.sprintf "%s %d, %d" month day year
