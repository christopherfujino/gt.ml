open Common
open Git_foo

let identifiers =
  let identifiers' : (string, Runtime.t) Hashtbl.t =
    (* random is an optional, named parameter *)
    let random = false in
    Hashtbl.create ~random 20
  in
  Hashtbl.add identifiers' "print"
    (`Function
      (fun es ->
        match es with
        | e :: [] -> `String (Ast.to_string e)
        | _ -> raise Unreachable));
  Hashtbl.add identifiers' "HEAD"
    (`Function
      (fun es ->
        match es with
        | [] ->
            let head = get_head () in
            let commit = Runtime.Commit.of_store_value head in
            `Commit commit
        | _ -> raise (YoloDawg "wrong number of arguments")));
  identifiers'
