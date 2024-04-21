open Main

(** A SHA-1 based unix filesystem git store *)
module Store = Git_unix.Store

(** A search module based on SHA-1 digests and our [Store] *)
module Search = Git.Search.Make (Digestif.SHA1) (Store)

let store =
  let store_p = Store.v (Fpath.v (Sys.getcwd ())) in
  let store_res = Lwt_main.run store_p in
  Result.get_ok store_res

(** This is just a test *)
let () =
  let hash_res_p = Store.Ref.resolve store Git.Reference.head in
  let store_val_res_p = Lwt_result.bind hash_res_p (Store.read store) in
  let store_val_res = Lwt_main.run store_val_res_p in
  let store_val = Result.get_ok store_val_res in
  Store.Value.pp Format.std_formatter store_val;
  print_endline "";
  match store_val with
  | Commit c -> begin
    let committer = Store.Value.Commit.committer c in
    let unix_time, _ = committer.date in
    Printf.printf "commit time was %s" (Int64.to_string unix_time)
  end
  | _ -> raise Unreachable

let get_head_async () : Store.Value.t Lwt.t =
  let open Lwt_result.Syntax in
  (* get store located in current root's .git folder *)
  let store_val =
    (* find obj-id pointed at by main branch (reference) *)
    let* commit_id = Store.Ref.resolve store Git.Reference.head in
    Store.read store commit_id
  in
  (* This will throw if not Ok *)
  Lwt.map Result.get_ok store_val

let get_head () =
  let rev_p = get_head_async () in
  Lwt_main.run rev_p

let rec iter store_val f =
  f store_val;
  let parent_hash = get_parent store_val in
  match parent_hash with None -> () | Some h -> iter h f

and get_parent (store_val : Store.hash) : Search.hash option =
  let val_res_prom = Store.read store store_val in
  let val_res = Lwt_main.run val_res_prom in
  let store_val = Result.get_ok val_res in
  let commit = match store_val with
  | Commit hash -> hash
  | _ -> raise Unreachable in
  let parents = Store.Value.Commit.parents commit in
  (* TODO handle multiple parents *)
  match parents with
  | [] -> None
  | h :: [] -> Some h
  | _ :: _ -> raise Todo
