open Gt.Git_foo

let () =
  let log_lwt_result = log () in
  let log_result = Lwt_main.run log_lwt_result in
  (* This will throw if not Ok *)
  let hashes_p = Result.get_ok log_result in
  let hashes = Lwt_main.run hashes_p in
  let hashes_s = List.map Store.Hash.to_hex hashes in
  List.iter print_endline hashes_s

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
