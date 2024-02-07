type expr =
  | Number of float
  | String of string
  | Invocation of string * (expr list)
  | IdentifierRef of string
