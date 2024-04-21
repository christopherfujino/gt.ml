open Main

let init commit =
  let open Git_foo in
  let hash = Store.Value.digest commit in
  let _hash_str = Store.Hash.to_hex hash in
  raise (YoloDawg "must implement returning a commit record")

let stdlib commit func_name =
  let open Runtime in
  match func_name with
  | "hash" -> fun _ -> Runtime.String commit.hash
  | _ -> raise Unreachable
