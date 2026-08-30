
(* LOWEST TO HIGHEST
Name	        Operators	    Associates
---------------------------------------
Equality	    == !=	        Left
Comparison	  > >= < <=	    Left
Term	        - +	          Left
Factor	      / *	          Left
Unary	        ! -	          Right
*)

(*
expression     → equality ;
equality       → comparison ( ( "!=" of ast * ast | "==" ) comparison )* ;
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term           → factor ( ( "-" | "+" ) factor )* ;
factor         → unary ( ( "/" | "*" ) unary )* ;
unary          → ( "!" | "-" ) unary
               | primary ;
primary        → NUMBER | STRING | "true" | "false" | "nil"
               | "(" expression ")" ;
*)
open Scanner
open Ppx_deriving_runtime
type binary_perator = IS_EQAL  | IS_NEQ | LT | LEQ | GT | GEQ | ADD | SUB | MULT | DIV
type unary_operator = NOT | NEG
type litteral = NUMBER_VALUE | STRING_VALUE | BOOL_TRUE | BOOL_FALSE | NIL_VALUE

type expression =
  | Litteral
  | Unary_operator of expression
  | Binary_perator of expression * expression
  | Grouping of expression

;;

type ast =
  | EMPTY
  (*Binary operators*)
  | IS_EQAL of ast * ast
  | IS_NEQ of ast * ast
  | LT of ast * ast
  | LEQ of ast * ast
  | GT of ast * ast
  | GEQ of ast * ast
  | ADD of ast * ast
  | SUB of ast * ast
  | MULT of ast * ast
  | DIV of ast * ast

  (*Unary operators*)
  |NOT of ast
  |NEG of ast

 (*Literals*)
 | NUMBER_VALUE of float
 | STRING_VALUE of string
 | BOOL_TRUE
 | BOOL_FALSE
 | NIL_VALUE


let is_primary (token:token) =
  match token.token_type with
  | NIL -> true
  | TRUE -> true
  | FALSE -> true
  | VAR -> true
  | IDENTIFIER -> true
  | STRING -> true
  | _ -> false

let is_compare (token:token) =
  match token.token_type with
  | GREATER -> true
  | GREATER_EQUAL -> true
  | LESS -> true
  | LESS_EQUAL -> true
  | _ -> false

type tok_l = Scanner.token list [@@deriving show]
let rec parse_parent (tokens: Scanner.token list) =
  print_endline ("=========Parsing parentheses==========");
  print_endline (show_tok_l tokens) ;
  match tokens with
  | [] -> failwith "Incorrect syntax : expected end of parentheses"
  | t :: tx when t.token_type == RIGHT_PAREN -> [], tx
  | t :: tx -> let prev, next = (parse_parent tx) in t::prev, next

let rec parse_expression (tokens: Scanner.token list) : ast =

  let rec parse_primary tokens ast =
  print_endline "Parse primary";
  match tokens with
  | [] -> ast
  | token::tx when token.token_type == LEFT_PAREN ->
      let exp_parent, next = parse_parent tokens in
      parse_primary next (parse_expression exp_parent)
  | token::tx -> (
    match token.token_type with
    | NIL -> NIL_VALUE
    | TRUE -> BOOL_TRUE
    | FALSE -> BOOL_FALSE
    | VAR | IDENTIFIER-> failwith "Not implemented yet"
    | STRING -> (match token.literal with
          | None -> failwith "Error"
          | Some (STRING_LITERAL v) -> (STRING_VALUE v)
          | _ -> failwith "Wrong string literal")
    | NUMBER -> (match token.literal with
          | None -> failwith "Error"
          | Some (NUMBER_LITERAL v) -> (NUMBER_VALUE v)
          | _-> failwith "Wrong number literal" )
    | _ -> parse_expression tokens)

in let parse_unary tokens ast =
  let rec parse_unary_aux tokens ast acc =
    (*print_endline "Parse unary";*)
  match tokens with
  | [] -> parse_primary acc ast
  | token::tx when token.token_type == BANG -> NOT (parse_unary_aux tx ast acc)
  | token::tx when token.token_type == MINUS -> NEG (parse_unary_aux tx ast acc)
  | token::tx -> parse_unary_aux tx ast (token::acc)
  in parse_unary_aux tokens ast []

in let parse_factor tokens ast =
  let rec parse_factor_aux tokens ast acc =
  print_endline "Parse factor";
  match tokens with
  | [] -> parse_unary acc ast
  | token::tx when token.token_type == STAR -> MULT (parse_unary acc ast, (parse_unary tx EMPTY))
  | token::tx when token.token_type == SLASH -> DIV (parse_unary acc ast, (parse_unary tx EMPTY))
  | token::tx -> parse_factor_aux tx ast (token::acc)
  in parse_factor_aux tokens ast []

in let parse_term tokens ast =
  let rec parse_term_aux tokens ast acc =
  print_endline "Parse term";
  match tokens with
  | [] -> parse_factor acc ast
  | token::tx when token.token_type == PLUS -> ADD (parse_factor acc ast, (parse_factor tx EMPTY))
  | token::tx when token.token_type == MINUS -> SUB (parse_factor acc ast, (parse_factor tx EMPTY))
  | token::tx -> parse_term_aux tx ast (token::acc)
  in parse_term_aux tokens ast []

in let parse_comparison tokens ast =
  let rec parse_comparison_aux tokens ast acc =
  print_endline "Parse comparison";
  match tokens with
  | [] -> parse_term acc ast
  | token::tx -> match token.token_type with
    | GREATER -> GT (parse_term acc ast, (parse_term tx EMPTY))
    | GREATER_EQUAL -> GEQ (parse_term acc ast, (parse_term tx EMPTY))
    | LESS -> LT (parse_term acc ast, (parse_term tx EMPTY))
    | LESS_EQUAL -> LEQ (parse_term acc ast, (parse_term tx EMPTY))
    | _ -> parse_comparison_aux tx ast (token::acc)
  in parse_comparison_aux tokens ast []

in
  let rec parse_equality tokens ast acc =
  match tokens with
  | [] -> parse_comparison acc ast
  | token::tx when token.token_type == EQUAL_EQUAL -> IS_EQAL (parse_comparison acc ast, parse_equality tx EMPTY [])
  | token::tx when token.token_type == BANG_EQUAL -> IS_NEQ (parse_comparison acc ast, parse_equality tx EMPTY [])
  | token::tx -> parse_equality tx ast (token::acc)

in parse_equality tokens EMPTY [];;
