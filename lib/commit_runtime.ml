open Common

let commit_of_store_value store_value =
  let open Git_foo in
  let hash = Store.Value.digest store_value in
  let hash_str = Store.Hash.to_hex hash in
  match store_value with
  | Commit c ->
      let committer = Store.Value.Commit.committer c in
      let open Runtime.Commit in
      { committer = User.of_git_user committer; revision = hash_str }
  | Blob _ -> raise Unreachable
  | Tree _ -> raise Unreachable
  | Tag _ -> raise Unreachable

let stdlib commit func_name =
  let open Runtime.Commit in
  match func_name with
  | "revision" -> fun _ -> Runtime.String commit.revision
  | "date" -> fun _ -> Runtime.Date commit.committer.date
  | _ -> raise (YoloDawg func_name)
