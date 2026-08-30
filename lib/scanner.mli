type tokenType =
  | LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACE
  | RIGHT_BRACE
  | COMMA
  | DOT
  | MINUS
  | PLUS
  | SEMICOLON
  | SLASH
  | STAR
  | BANG
  | BANG_EQUAL
  | EQUAL
  | EQUAL_EQUAL
  | GREATER
  | GREATER_EQUAL
  | LESS
  | LESS_EQUAL
  | IDENTIFIER
  | STRING
  | NUMBER
  | AND
  | CLASS
  | ELSE
  | FALSE
  | FUN
  | FOR
  | IF
  | NIL
  | OR
  | PRINT
  | RETURN
  | SUPER
  | THIS
  | TRUE
  | VAR
  | WHILE
  | EOF
  | BOF
  [@@deriving show]
type literal_type =
  | STRING_LITERAL of string
  | NUMBER_LITERAL of float
  [@@deriving show]
type token = {
  token_type : tokenType;
  lexeme : string;
  literal : literal_type option;
  line : int;
}[@@deriving show]

type scanner_ctx = {
  source : string;
  start : int;
  current : int;
  line : int;
  tokens : token list;
}

val is_at_end : scanner_ctx -> bool
val current_char : scanner_ctx -> char option
val advance : scanner_ctx -> scanner_ctx
val newline : scanner_ctx -> scanner_ctx
val add_token : tokenType -> literal_type option -> scanner_ctx -> scanner_ctx
val is_next : char -> scanner_ctx -> bool
val skip_line : scanner_ctx -> scanner_ctx
val scan_string : scanner_ctx -> scanner_ctx
val scan_token : scanner_ctx -> scanner_ctx
val scan_source : scanner_ctx -> scanner_ctx
val init_scanner : string -> scanner_ctx
