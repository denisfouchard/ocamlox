open Scanner
open Parser

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


let rec print_ast (t:ast) =
  match t with

  (*Binary operators*)
  | IS_EQAL (t1, t2)-> Printf.sprintf "IS_EQ(%S, %S)" (print_ast t1) (print_ast t2)
  | IS_NEQ (t1, t2)-> Printf.sprintf "IS_NEQ(%S, %S)" (print_ast t1) (print_ast t2)
  | LT (t1, t2)-> Printf.sprintf "LT(%S, %S)" (print_ast t1) (print_ast t2)
  | LEQ (t1, t2)-> Printf.sprintf "LEQ(%S, %S)" (print_ast t1) (print_ast t2)
  | GT (t1, t2)-> Printf.sprintf "GT(%S, %S)" (print_ast t1) (print_ast t2)
  | GEQ (t1, t2)-> Printf.sprintf "GEQ(%S, %S)" (print_ast t1) (print_ast t2)
  | ADD (t1, t2)-> Printf.sprintf "ADD(%S, %S)" (print_ast t1) (print_ast t2)
  | SUB (t1, t2)-> Printf.sprintf "SUB(%S, %S)" (print_ast t1) (print_ast t2)
  | MULT (t1, t2)-> Printf.sprintf "MULT(%S, %S)" (print_ast t1) (print_ast t2)
  | DIV (t1, t2)-> Printf.sprintf "DIV(%S, %S)" (print_ast t1) (print_ast t2)

  (*Unary operators*)
  |NOT t1 -> Printf.sprintf "Not(%S)" (print_ast t1)
  |NEG t1 -> Printf.sprintf "Minus(%S)" (print_ast t1)

 (*Literals*)
 | EMPTY -> "EMPTY"
 | NUMBER_VALUE v -> Printf.sprintf "Number(%f)" v
 | STRING_VALUE s -> Printf.sprintf "String(%S)" s
 | BOOL_TRUE -> "True"
 | BOOL_FALSE -> "False"
 | NIL_VALUE -> "Nil"
