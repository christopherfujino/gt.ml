let parse input = (Parser.program Lexer.read) (Lexing.from_string input)

exception YoloDawg of string

exception Unreachable

exception Todo
