type expr =
  | Number of float
  | String of string
  | Invocation of string * expr
  | IdentifierRef of string
