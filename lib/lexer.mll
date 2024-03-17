(* header *)
{
open Parser

exception SyntaxError of string
}

(* identifiers *)
(* TODO handle newlines separately to track line numbers *)
let white = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let number = '-'? ((['1'-'9'] digit*) | '0') ('.' digit+)? (['e' 'E'] ('-'|'+')? digit+)?
let comment = "//" [^ '\n']+
let identifier = ['a'-'z' 'A'-'Z']+
(*
let hex = ['0'-'9' 'a'-'f' 'A'-'F']
*)

(* rule and parse are keywords *)
rule read =
  parse
  (* means if `white` matches, call the read rule again and return its
     results--i.e. skip this match *)
  | white     { read lexbuf }
  | comment   { read lexbuf }
  | identifier { IDENTIFIER (Lexing.lexeme lexbuf)}
  | '('       { OPEN_PAREN }
  | ')'       { CLOSE_PAREN }
  | '.'       { DOT }
  (*
  | ','       { COMMA }
  | '['       { OPEN_BRACKET }
  | ']'       { CLOSE_BRACKET }
  | '{'       { OPEN_CURLY }
  | '}'       { CLOSE_CURLY }
  | ':'       { COLON }
  *)
  (* TODO should we store the actual input string in case of scientific notation? *)
  | number    { NUM (float_of_string (Lexing.lexeme lexbuf))}
  | '"'       { read_string (Buffer.create 17) lexbuf }
  | ','       { COMMA }
  (*
  | "null"    { NULL }
  | "true"    { TRUE }
  | "false"   { FALSE }
  *)
  (* Here `eof` is a special regex built into ocamllex *)
  | eof       { EOF }
  | _ as c { failwith (Printf.sprintf "unexpected character: %C" c) }

and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  (*
  | '\\' 'u' hex hex hex hex
    {
      (* TODO to make a valid ocaml utf-8 string, I think we'd need to
       * manipulate bytes *)
      Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
    *)
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | eof { raise (SyntaxError "String is not terminated") }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
