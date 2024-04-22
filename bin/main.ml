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
    (`Function
      (fun es ->
        match es with
        | e :: [] -> `String (Ast.to_string e)
        | _ -> raise Unreachable));
  Hashtbl.replace identifiers' "HEAD"
    (`Function
      (fun es ->
        match es with
        | [] ->
            let head = get_head () in
            let commit = Runtime.Commit.of_store_value head in
            `Commit commit
        | _ -> raise (YoloDawg "wrong number of arguments")));
  identifiers'

let () =
  List.iter
    (fun program ->
      let expr = parse program in
      print_endline program;
      let open Interpreter in
      interpret { identifiers } expr |> Runtime.to_string |> print_endline)
    programs
