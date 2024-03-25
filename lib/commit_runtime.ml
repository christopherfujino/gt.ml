open Main

let init commit =
  let open Git_foo in
  let hash = Store.Value.digest commit in
  let hash_str = Store.Hash.to_hex hash in
  let open Runtime in
  { hash = hash_str ; date = commit }

let stdlib commit func_name =
  let open Runtime in
  match func_name with
  | "hash" -> fun _ -> Runtime.String commit.hash
  | _ -> raise Unreachable
