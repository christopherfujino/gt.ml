open Gt
open Common
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
  let identifiers' =
    (* random is an optional, named parameter *)
    let random = false in
    Hashtbl.create ~random 20
  in
  Hashtbl.replace identifiers' "print"
    (Runtime.Function
       (fun es ->
         match es with
         | e :: [] -> String (Ast.to_string e)
         | _ -> raise Unreachable));
  Hashtbl.replace identifiers' "HEAD"
    (Function
       (fun es ->
         match es with
         | [] -> Commit (() |> get_head |> Runtime.Commit.of_store_value)
         | _ -> raise (YoloDawg "wrong number of arguments")));
  identifiers'

let () =
  List.iter
    (fun program ->
      let expr = parse program in
      Printf.printf "%s\n" program;
      let open Interpreter in
      interpret { identifiers } expr |> Runtime.to_string |> print_endline)
    programs
