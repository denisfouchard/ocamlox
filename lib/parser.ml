
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
unary          → ( "!" | "-" ) unary| primary ;
primary        → NUMBER | STRING | "true" | "false" | "nil"
               | "(" expression ")" ;
*)

(*
program        → statement* EOF ;

statement      → exprStmt
               | printStmt ;

exprStmt       → expression ";" ;
printStmt      → "print" expression ";" *)

open Scanner
open Ppx_deriving_runtime
type binary_perator = IS_EQAL  | IS_NEQ | LT | LEQ | GT | GEQ | ADD | SUB | MULT | DIV
type unary_operator = NOT | NEG
type litteral = NUMBER_VALUE | STRING_VALUE | BOOL_TRUE | BOOL_FALSE | NIL_VALUE


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

 (*Statements*)
 |StatementSequence of ast list
 |Statement of ast
 |ExpressionStatement of ast
 |PrintStatement of ast

 |VariableDeclaration of {name:string; value:ast}
 |VariableAccess of string
 [@@deriving show]

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

let rec parse_expression tokens =
  parse_equality tokens

and parse_equality tokens =
  let acc, tokens = parse_comparison tokens in
  let rec loop acc tokens =
    match tokens with
    | { token_type = EQUAL_EQUAL; _ } :: next ->
        let right, next = parse_comparison next in
        loop (IS_EQAL (acc, right)) next
    | { token_type = BANG_EQUAL; _ } :: next ->
        let right, next = parse_comparison next in
        loop (IS_NEQ (acc, right)) next
    | _ -> acc, tokens
  in
  loop acc tokens

and parse_comparison tokens =
  let acc, tokens = parse_term tokens in
  let rec loop acc tokens =
    match tokens with
    | { token_type = GREATER; _ } :: next ->
        let right, next = parse_term next in
        loop (GT (acc, right)) next
    | { token_type = GREATER_EQUAL; _ } :: next ->
        let right, next = parse_term next in
        loop (GEQ (acc, right)) next
    | { token_type = LESS; _ } :: next ->
        let right, next = parse_term next in
        loop (LT (acc, right)) next
    | { token_type = LESS_EQUAL; _ } :: next ->
        let right, next = parse_term next in
        loop (LEQ (acc, right)) next
    | _ -> acc, tokens
  in
  loop acc tokens

and parse_term tokens =
  let acc, tokens = parse_factor tokens in
  let rec loop acc tokens =
    match tokens with
    | { token_type = PLUS; _ } :: next ->
        let right, next = parse_factor next in
        loop (ADD (acc, right)) next
    | { token_type = MINUS; _ } :: next ->
        let right, next = parse_factor next in
        loop (SUB (acc, right)) next
    | _ -> acc, tokens
  in
  loop acc tokens

and parse_factor tokens =
  let acc, tokens = parse_unary tokens in
  let rec loop acc tokens =
    match tokens with
    | { token_type = STAR; _ } :: next ->
        let right, next = parse_unary next in
        loop (MULT (acc, right)) next
    | { token_type = SLASH; _ } :: next ->
        let right, next = parse_unary next in
        loop (DIV (acc, right)) next
    | _ -> acc, tokens
  in
  loop acc tokens

and parse_unary tokens =
  match tokens with
  | { token_type = BANG; _ } :: next ->
      let expr, next = parse_unary next in
      NOT expr, next
  | { token_type = MINUS; _ } :: next ->
      let expr, next = parse_unary next in
      NEG expr, next
  | _ -> parse_primary tokens

and parse_primary tokens =
  match tokens with
  | [] -> failwith "Unexpected end of input"
  | { token_type = LEFT_PAREN; _ } :: next ->
      let expr, next = parse_expression next in
      (match next with
        | { token_type = RIGHT_PAREN; _ } :: next' -> expr, next'
       | _ -> failwith "Expected ')' after expression")
  | { token_type = NIL; _ } :: next -> NIL_VALUE, next
  | { token_type = TRUE; _ } :: next -> BOOL_TRUE, next
  | { token_type = FALSE; _ } :: next -> BOOL_FALSE, next
  | { token_type = STRING; literal = Some (STRING_LITERAL v); _ } :: next -> STRING_VALUE v, next
  | { token_type = NUMBER; literal = Some (NUMBER_LITERAL v); _ } :: next -> NUMBER_VALUE v, next
  (* variable access*)
  | token:: next when token.token_type == IDENTIFIER -> parse_var token, next
  | token :: _ -> failwith ("[ParsingError]Invalid token: " ^ show_tokenType token.token_type)
and parse_var token =
  match token.literal with
  | Some (STRING_LITERAL name) -> VariableAccess name
  | _-> failwith("[ParsingError] Unbound variable")

let rec parse_semicol tokens acc=
  match tokens with
  |[] -> failwith ("Unexpected EOF, missing ;")
  |{token_type = SEMICOLON; _}::next -> acc, next
  |tok::tx -> let st, next = parse_semicol tx [] in tok::st, next

let rec parse_program tokens =
  parse_declaration_sequence tokens

and parse_declaration_sequence tokens =
  let rec loop tokens =
  let statement_tokens, next = parse_semicol tokens [] in
  let ast = parse_declaration statement_tokens in
    match next with
    | [] -> failwith("[StatementParsingError]Unexpected EOF, missing EOF token")
    | { token_type = EOF; _}::_ -> [ast]
    | _ -> ast :: (loop next)

  in StatementSequence (loop tokens)

and parse_declaration tokens =
  match tokens with
  |[] -> EMPTY
  |{ token_type = VAR; _}::var_decl -> parse_variable_declaration var_decl
  |{ token_type = EOF; _}::_ -> EMPTY
  |{ token_type = PRINT; _}::q -> let expr, _ = parse_expression q in
    (PrintStatement expr)
  | _ ->  let expr, _ = parse_expression tokens in
    (ExpressionStatement expr)

and parse_variable_declaration expr =
  match expr with
  |identifier::{token_type = EQUAL; _}::decl_expr
    when identifier.token_type == IDENTIFIER ->
    let decl_t,_  = parse_expression decl_expr in
    VariableDeclaration ({name=identifier.lexeme;value=decl_t})
  |_-> failwith("[VariableDeclarationError] Wrong variable declaration")

let parse tokens =
  let ast, remaining = parse_expression tokens in
  match remaining with
  | [] | [{ token_type = EOF; _ }] -> ast
  | t :: _ -> failwith ("Unexpected trailing token: " ^ show_tokenType t.token_type)
