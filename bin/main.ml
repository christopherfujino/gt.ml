open Gt.Main

let programs =
  [ {|1|}; {|"Hello, world!"|}; {|HEAD|}; {|print("Hello, world!")|} ]

let rec print_programs = function
  | [] -> ()
  | h :: t ->
      let v = parse h in
      Printf.printf "%25s -> %s\n" h (printer v);
      print_programs t

let parsed_programs = List.map parse programs

let () = print_programs programs

let () = List.iter (interpret {foo = () }) parsed_programs
