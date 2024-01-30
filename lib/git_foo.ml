module Store = Git_unix.Store
module Search = Git.Search.Make (Digestif.SHA1) (Store)

let get_head () : Store.Value.t Lwt.t =
  let open Lwt_result.Syntax in
  (* get store located in current root's .git folder *)
  let store_val =
    let* store = Store.v (Fpath.v (Sys.getcwd ())) in
    (* find obj-id pointed at by main branch (reference) *)
    let* commit_id = Store.Ref.resolve store Git.Reference.head in
    Store.read store commit_id
  in
  (* This will throw if not Ok *)
  Lwt.map Result.get_ok store_val

exception Foo

let get_parent (store_val : Store.Value.t) : Search.hash =
  let commit = match store_val with
  | Commit hash -> hash
  | _ -> raise Foo in
  let parents = Store.Value.Commit.parents commit in
  (* TODO handle multiple parents *)
  List.hd parents
