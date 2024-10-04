open Common

type t = (string, Runtime.t) Hashtbl.t

let create_2 git_foo =
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
        | [ e ] ->
            let contents =
              match e with String s -> s | any -> Ast.to_string any
            in
            `String contents
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

let seq_of_list l =
  let rec f idx () =
    if idx >= List.length l then Seq.Nil else Cons (List.nth l idx, f (idx + 1))
  in
  f 0

let create git_foo =
  let module Git_foo = (val git_foo : Git_foo.Git_type) in
  let seq =
    seq_of_list
      [
        ( "print",
          `Function
            (fun es ->
              match es with
              | [ e ] ->
                  let contents =
                    match e with Ast.String s -> s | any -> Ast.to_string any
                  in
                  `String contents
              | _ -> raise Unreachable) );
        ( "HEAD",
          `Function
            (fun es ->
              match es with
              | [] ->
                  let head = Git_foo.get_head () in
                  let commit = Runtime.Commit.of_store_value head in
                  `Commit commit
              | _ -> raise (YoloDawg "wrong number of arguments")) );
      ]
  in
  Hashtbl.of_seq seq
