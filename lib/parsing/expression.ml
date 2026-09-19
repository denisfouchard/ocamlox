open Scanner
open Ast

let parse_paren tokens (add_semicol:bool) =
  let rec parse_paren_aux tokens p =
    match tokens with
      | [] | {token_type = EOF; _}::_ -> failwith ("Unexpected EOF, missing }")
      | tok::next when tok.token_type == RIGHT_PAREN ->
          if p ==0 then
            (match next with
              | {token_type = SEMICOLON;_}::nnext -> [], nnext
              | _-> [], next
            )
          else
            let st, next = (parse_paren_aux next (p-1) ) in
            tok::st, next
      | last::right_PAREN::next
        when last.token_type != SEMICOLON
        && right_PAREN.token_type == RIGHT_PAREN
        && add_semicol ->
        parse_paren_aux (last::(semi_col_token ())::right_PAREN::next) p
      | tok::next when tok.token_type == LEFT_BRACE ->
      let st, next = (parse_paren_aux next (p+1)) in tok::st, next
      | tok::next->
      let st, next = (parse_paren_aux next p) in tok::st, next
  and semi_col_token () = {token_type=SEMICOLON; lexeme=";";literal=None;line=0}
  in parse_paren_aux tokens 0

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
    | _ -> parse_call tokens

and parse_call tokens =
  let acc, next = parse_primary tokens in
  match next with
    | { token_type = LEFT_PAREN; _ }::next ->
      (
      let arg_tokens, next = parse_paren next false in
      let args = parse_args arg_tokens in
        if List.length args > 255
        then failwith ("[ParsingError] Too many args (max 255")
        else
        FunctionCall {callee=acc;arguments=args}, next
      )
    | _ -> acc, next
and parse_primary tokens =
  match tokens with
    | [] -> EMPTY, []
    | { token_type = LEFT_PAREN; _ } :: next ->
        let expr, next = parse_expression next in
        (match next with
          | { token_type = RIGHT_PAREN; _ } :: next' -> expr, next'
          | _ -> failwith "Expected ')' after expression"
        )
    | { token_type = NIL; _ } :: next -> NIL_VALUE, next
    | { token_type = TRUE; _ } :: next -> BOOL_TRUE, next
    | { token_type = FALSE; _ } :: next -> BOOL_FALSE, next
    | { token_type = STRING;
        literal = Some (STRING_LITERAL v); _
      } :: next -> STRING_VALUE v, next
    | { token_type = NUMBER;
        literal = Some (NUMBER_LITERAL v); _
      } :: next -> NUMBER_VALUE v, next
    (* variable access*)
    | token:: next when token.token_type == IDENTIFIER -> parse_var token, next
    | token :: _ -> failwith ("[ParsingError] Invalid token "
            ^ token.lexeme
            ^ " at line:"^(string_of_int token.line))

and parse_var token =
  match token.literal with
    | Some (STRING_LITERAL name) -> VariableAccess name
    | _-> failwith("[ParsingError] Unbound variable")

and parse_args tokens =
  let arg, next = parse_expression tokens in
  match next with
  | [] -> (if arg == EMPTY then [] else [arg])
  | { token_type = COMMA; _}::next_args ->
    arg::(parse_args next_args)
  |_ -> print_endline ("Next tokens : " ^show_tok_l next); failwith("[ParsingError: Uncorrectly formated args")



let parse tokens =
  let ast, remaining = parse_expression tokens in
  match remaining with
    | [] | [{ token_type = EOF; _ }] -> ast
    | t :: _ -> failwith ("Unexpected trailing token: " ^ show_tokenType t.token_type)
