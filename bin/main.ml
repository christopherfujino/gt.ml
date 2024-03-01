open Gt
open Main
open Git_foo

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
  Hashtbl.replace identifiers "HEAD" (Interpreter.Commit hash_str)

let () =
  Hashtbl.replace identifiers "print"
    (Function
       (fun es ->
         match es with
         | e :: [] -> StringValue (Interpreter.expr_to_string e)
         | _ -> raise YoloDawg));
  let rev_p = get_head () in
  let rev_value = Lwt_main.run rev_p in
  let hash = Store.Value.digest rev_value in
  let hash_str = Store.Hash.to_hex hash in
  Hashtbl.replace identifiers "HEAD"
    (Function
       (fun es -> match es with [] -> StringValue hash_str | _ -> raise YoloDawg))

let () =
  List.iter
    (fun program ->
      let expr = parse program in
      Printf.printf "%s\n" program;
      let open Interpreter in
      interpret { identifiers } expr;
      print_endline "")
    programs
