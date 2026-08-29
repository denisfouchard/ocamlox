open Ocamlox.Scanner

let string_of_token_type = function
  | LEFT_PAREN -> "LEFT_PAREN"
  | RIGHT_PAREN -> "RIGHT_PAREN"
  | LEFT_BRACE -> "LEFT_BRACE"
  | RIGHT_BRACE -> "RIGHT_BRACE"
  | COMMA -> "COMMA"
  | DOT -> "DOT"
  | MINUS -> "MINUS"
  | PLUS -> "PLUS"
  | SEMICOLON -> "SEMICOLON"
  | SLASH -> "SLASH"
  | STAR -> "STAR"
  | BANG -> "BANG"
  | BANG_EQUAL -> "BANG_EQUAL"
  | EQUAL -> "EQUAL"
  | EQUAL_EQUAL -> "EQUAL_EQUAL"
  | GREATER -> "GREATER"
  | GREATER_EQUAL -> "GREATER_EQUAL"
  | LESS -> "LESS"
  | LESS_EQUAL -> "LESS_EQUAL"
  | IDENTIFIER -> "IDENTIFIER"
  | STRING -> "STRING"
  | NUMBER -> "NUMBER"
  | AND -> "AND"
  | CLASS -> "CLASS"
  | ELSE -> "ELSE"
  | FALSE -> "FALSE"
  | FUN -> "FUN"
  | FOR -> "FOR"
  | IF -> "IF"
  | NIL -> "NIL"
  | OR -> "OR"
  | PRINT -> "PRINT"
  | RETURN -> "RETURN"
  | SUPER -> "SUPER"
  | THIS -> "THIS"
  | TRUE -> "TRUE"
  | VAR -> "VAR"
  | WHILE -> "WHILE"
  | EOF -> "EOF"
  | BOF -> "BOF"

let print_token tok =
  let lit_str =
    match tok.literal with
    | None -> "None"
    | Some (STRING_LITERAL s) -> Printf.sprintf "String(%S)" s
    | Some (NUMBER_LITERAL n) -> Printf.sprintf "Number(%f)" n
  in
  Printf.printf "  [type: %-13s | lexeme: %-6S | literal: %s | line: %d]\n"
    (string_of_token_type tok.token_type)
    tok.lexeme lit_str tok.line

let () =
  print_endline "=== Running Scanner Print Test ===";
  let source = "(=)<><>({})//this is a zzcomment\n=\"pizi\"" in
  Printf.printf "Input source:\n%s\n\n" source;

  let context =
    { source; start = 0; current = -1; line = 0; tokens = [] }
  in
  let result = scan_source context in

  print_endline "Tokens scanned:";
  List.rev result.tokens |> List.iter print_token;
  print_endline "=== Test Complete ==="
