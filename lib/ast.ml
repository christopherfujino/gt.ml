type expr =
  | Number of float
  | String of string
  (* TODO should be a list of expressions *)
  | Invocation of string * (expr list)
  | IdentifierRef of string
