open Gt
open Main
open Git_foo

let programs =
  [
    {|1|};
    {|"Hello, world!"|};
    {|HEAD()|};
    {|HEAD().date()|};
    {|HEAD|};
    {|print("Hello, world!")|};
  ]

let identifiers =
  (* random is an optional, named parameter *)
  let random = false in
  Hashtbl.create ~random 20

let () =
  Hashtbl.replace identifiers "print"
    (Runtime.Function
       (fun es ->
         match es with
         | e :: [] -> String (Ast.to_string e)
         | _ -> raise Unreachable));
  Hashtbl.replace identifiers "HEAD"
    (Function
       (fun es ->
         match es with
         | [] -> Commit (() |> get_head |> Commit_runtime.commit_of_store_value)
         | _ -> raise (YoloDawg "wrong number of arguments")))

let () =
  List.iter
    (fun program ->
      let expr = parse program in
      Printf.printf "%s\n" program;
      let open Interpreter in
      interpret { identifiers } expr |> Runtime.to_string |> print_endline)
    programs
