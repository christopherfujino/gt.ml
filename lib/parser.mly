%{
  open Ast
%}

(* Declarations *)
%token <float> NUM
%token <string> STRING
%token <string> IDENTIFIER
%token EOF
%token COMMA
%token OPEN_PAREN
%token CLOSE_PAREN

%start <expr> program

%%

program:
  | value = expr; EOF { value }

expr:
  | value = NUM        { Number value }
  | value = STRING     { String value }
  | id = IDENTIFIER { IdentifierRef id }
  | id = IDENTIFIER; OPEN_PAREN ; e = expr ; CLOSE_PAREN { Invocation (id, e) }
