let parse input = (Parser.program Lexer.read) (Lexing.from_string input)

exception YoloDawg

exception Todo of string
