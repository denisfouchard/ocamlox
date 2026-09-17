open Scanner
open Ast

let rec parse_expression tokens =
  parse_logic_or tokens

and parse_logic_or tokens =
  let acc, tokens = parse_logic_and tokens in
  let rec loop acc tokens =
    match tokens with
    | { token_type = OR; _ } :: next ->
      let right, next = parse_logic_and next in
      loop (OR (acc, right)) next
    |_ -> acc, tokens
  in loop acc tokens

and parse_logic_and tokens =
  let acc, tokens = parse_equality tokens in
  let rec loop acc tokens =
    match tokens with
    | { token_type = AND; _ } :: next ->
      let right, next = parse_equality next in
      loop (AND (acc, right)) next
    |_ -> acc, tokens
  in loop acc tokens

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
  in loop acc tokens

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
  in loop acc tokens

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
  in loop acc tokens

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
          | _ -> failwith "Expected ')' after expression"
        )
    | { token_type = NIL; _ } :: next -> NIL_VALUE, next
    | { token_type = TRUE; _ } :: next -> BOOL_TRUE, next
    | { token_type = FALSE; _ } :: next -> BOOL_FALSE, next
    | { token_type = STRING; literal = Some (STRING_LITERAL v); _ } :: next -> STRING_VALUE v, next
    | { token_type = NUMBER; literal = Some (NUMBER_LITERAL v); _ } :: next -> NUMBER_VALUE v, next
    (* variable access*)
    | token:: next when token.token_type == IDENTIFIER -> parse_var token, next
    | token :: _ -> failwith ("[ParsingError] Invalid token " ^ token.lexeme ^ " at line:"^(string_of_int token.line))

and parse_var token =
  match token.literal with
    | Some (STRING_LITERAL name) -> VariableAccess name
    | _-> failwith("[ParsingError] Unbound variable")


let parse tokens =
  let ast, remaining = parse_expression tokens in
  match remaining with
    | [] | [{ token_type = EOF; _ }] -> ast
    | t :: _ -> failwith ("Unexpected trailing token: " ^ show_tokenType t.token_type)
