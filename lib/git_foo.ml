module Store = Git_unix.Store
module Search = Git.Search.Make (Digestif.SHA1) (Store)

let get_head () =
  let open Lwt_result.Syntax in
  (* get store located in current root's .git folder *)
  let store_val =
    let* store = Store.v (Fpath.v (Sys.getcwd ())) in
    (* find obj-id pointed at by main branch (reference) *)
    let* commit_id = Store.Ref.resolve store Git.Reference.head in
    Store.read store commit_id
  in
  Lwt.map Result.get_ok store_val
