(*
open Common
*)
module Store = Git_unix.Store
(** A SHA-1 based unix filesystem git store *)

module Search = Git.Search.Make (Digestif.SHA1) (Store)
(** A search module based on SHA-1 digests and our [Store] *)

module type Git_deps = sig
  val root : string
end

module type Git_type = sig
  val get_head : unit -> Store.Value.t
end

module Make (T : Git_deps) : Git_type = struct
  let store =
    let store_p = Store.v (Fpath.v T.root) in
    let store_res = Lwt_main.run store_p in
    Result.get_ok store_res

  let get_head_async () =
    let open Lwt_result.Infix in
    (* get store located in current root's .git folder *)
    let store_result =
      (* find obj-id pointed at by main branch (reference) *)
      Store.Ref.resolve store Git.Reference.head >>= Store.read store
    in
    Lwt.map (function Ok v -> v | Error _ -> failwith "TODO") store_result

  let get_head () =
    let rev_p = get_head_async () in
    Lwt_main.run rev_p
  (*
  let rec iter store_val f =
    f store_val;
    let parent_hash = get_parent store_val in
    match parent_hash with None -> () | Some h -> iter h f

  and get_parent (store_val : Store.hash) : Search.hash option =
    let val_res_prom = Store.read store store_val in
    let val_res = Lwt_main.run val_res_prom in
    let store_val = Result.get_ok val_res in
    let commit =
      match store_val with Commit hash -> hash | _ -> raise Unreachable
    in
    let parents = Store.Value.Commit.parents commit in
    (* TODO handle multiple parents *)
    match parents with [] -> None | h :: [] -> Some h | _ :: _ -> raise Todo
*)
end
