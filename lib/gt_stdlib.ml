open Common

type t = (string, Runtime.t) Hashtbl.t

let create git_foo =
  let module Git_foo = (val git_foo : Git_foo.Git_type) in
  let identifiers : t =
    (* random is an optional, named parameter *)
    (* TODO: use Hashtbl.of_seq *)
    Hashtbl.create ~random:false 20
  in
  Hashtbl.add identifiers "print"
    (`Function
      (fun es ->
        match es with
        | e :: [] -> `String (Ast.to_string e)
        | _ -> raise Unreachable));
  Hashtbl.add identifiers "HEAD"
    (`Function
      (fun es ->
        match es with
        | [] ->
            let head = Git_foo.get_head () in
            let commit = Runtime.Commit.of_store_value head in
            `Commit commit
        | _ -> raise (YoloDawg "wrong number of arguments")));
  identifiers
