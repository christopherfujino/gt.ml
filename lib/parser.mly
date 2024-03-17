%{
  (* open statements go here *)
%}

(* Declarations *)
%token <float> NUM
%token <string> STRING
%token <string> IDENTIFIER
%token EOF
%token COMMA
%token OPEN_PAREN
%token CLOSE_PAREN
%token DOT

%start <Ast.t> program

%%

program:
  | value = expr; EOF { value }

expr:
  | value = NUM        { `Number value }
  | value = STRING     { `String value }
  | id = IDENTIFIER { `IdentifierRef id }
  | id = IDENTIFIER; OPEN_PAREN ; es = args ; CLOSE_PAREN { `Invocation (id, es) }
  | id = IDENTIFIER; OPEN_PAREN ; CLOSE_PAREN { `Invocation (id, []) }
  | e = expr; DOT; id = IDENTIFIER; OPEN_PAREN; CLOSE_PAREN { `MethodInvocation (e, id, []) }

args:
  | e = expr { e :: [] }
  | { [] }
