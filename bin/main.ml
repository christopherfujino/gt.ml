open Gt.Git_foo

exception Foo of string

let () =
  let log_lwt_result = log () in
  let log_result = Lwt_main.run log_lwt_result in
  match log_result with
  | Ok s ->
      let hashes = Lwt_main.run s in
      List.iter (fun rev -> Store.Hash.to_hex rev |> print_endline) hashes
  | Error _ -> raise (Foo "got a Store.error: %s")

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
