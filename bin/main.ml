open Gt.Git_foo

let () =
  let val_p = get_head () in
  let store_val = Lwt_main.run val_p in
  Store.Value.digest store_val |> Store.Hash.to_hex |> print_endline;
  get_parent store_val |> Store.Hash.to_hex |> print_endline

(*
let programs =
  [ {|1|}; {|"Hello, world!"|}; {|HEAD|}; {|print("Hello, world!")|} ]

let identifiers =
  (* random is an optional, named parameter *)
  let random = false in
  Hashtbl.create ~random 20

let () = Hashtbl.replace identifiers "HEAD" (Commit "deadbeef")

let () =
  Hashtbl.replace identifiers "print"
    (Function (fun e -> StringValue (expr_to_string e)))

let () =
  List.iter
    (fun program ->
      let expr = parse program in
      Printf.printf "%25s -> %s\n" program (expr_to_string expr);
      interpret { foo = (); identifiers } expr)
    programs

*)
