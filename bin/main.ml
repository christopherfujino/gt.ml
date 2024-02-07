open Gt.Git_foo
open Gt.Main

let programs =
  [
    {|1|}; {|"Hello, world!"|}; {|HEAD()|}; {|HEAD|}; {|print("Hello, world!")|};
  ]

let identifiers =
  (* random is an optional, named parameter *)
  let random = false in
  Hashtbl.create ~random 20

let () =
  let rev_p = get_head () in
  let rev_value = Lwt_main.run rev_p in
  let hash = Store.Value.digest rev_value in
  let hash_str = Store.Hash.to_hex hash in
  Hashtbl.replace identifiers "HEAD" (Commit hash_str)

let () =
  Hashtbl.replace identifiers "print"
    (Function
       (fun es ->
         match es with
         | e :: [] -> StringValue (expr_to_string e)
         | _ -> raise Foo));
  let rev_p = get_head () in
  let rev_value = Lwt_main.run rev_p in
  let hash = Store.Value.digest rev_value in
  let hash_str = Store.Hash.to_hex hash in
  Hashtbl.replace identifiers "HEAD"
    (Function
       (fun es -> match es with [] -> StringValue hash_str | _ -> raise Foo))

let () =
  List.iter
    (fun program ->
      let expr = parse program in
      Printf.printf "%s\n" program;
      interpret { identifiers } expr;
      print_endline "")
    programs
