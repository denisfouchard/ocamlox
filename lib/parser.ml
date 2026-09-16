(* LOWEST TO HIGHEST
Name	        Operators	    Associates
---------------------------------------
Equality	    == !=	        Left
Comparison	  > >= < <=	    Left
Term	        - +	          Left
Factor	      / *	          Left
Unary	        ! -	          Right


program        → declaration* EOF ;

declaration    → varDecl
               | statement ;

statement      → exprStmt
              | ifStmt
              | whileStmt
              | forStmt
              | printStmt
              | block ;

ifStmt         → "if" "(" expression ")" statement
              ( "else" statement )? ;

whileStmt      -> "while" "(" expression ")" statement

block          → "{" declaration* "}" ;

exprStmt       → expression ";" ;
printStmt      → "print" expression ";"

expression     → assignment ;
assignment     → IDENTIFIER "=" assignment
               | logic_or ;
logic_or       → logic_and ( "or" logic_and )* ;
logic_and      → equality ( "and" equality )* ;

equality       → comparison ( ( "!=" of ast * ast | "==" ) comparison )* ;
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term           → factor ( ( "-" | "+" ) factor )* ;
factor         → unary ( ( "/" | "*" ) unary )* ;
unary          → ( "!" | "-" ) unary| primary ;
primary        → NUMBER | STRING | "true" | "false" | "nil"
               | "(" expression ")" ;

*)
open Scanner


type tok_l = Scanner.token list
[@@deriving show]

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

  (*Logical operations*)
  | OR of ast * ast
  | AND of ast * ast

  (*Unary operators*)
  |NOT of ast
  |NEG of ast

 (*Literals*)
 | NUMBER_VALUE of float
 | STRING_VALUE of string
 | BOOL_TRUE
 | BOOL_FALSE
 | NIL_VALUE

 (**DECLARATIONS**)
 (*Statements*)
 | StatementSequence of ast list

 | Statement of ast
 | ExpressionStatement of ast
 | PrintStatement of ast
 | IfStatement of {condition:ast;
                    then_branch:ast;
                    else_branch: ast option
                  }

 | WhileStatement of {condition:ast;
                     loop:ast}
 | ForStatement of {var_decl:ast;
                    loop_condition:ast;
                    incr:ast;
                    loop:ast
                  }

 | VariableDeclaration of {name:string; value:ast}
 | VariableAccess of string
 | VariableMutation of {name:string; value:ast}

 | Block of ast list
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




(** Helper functions for parsing statements**)
let rec parse_semicol tokens acc=
  match tokens with
    | [] | {token_type = EOF; _}::_ -> failwith ("Unexpected EOF, missing ;")
    | {token_type = SEMICOLON; _}::next -> acc, next
    | tok::tx -> let st, next = parse_semicol tx [] in tok::st, next



let parse_scope tokens =
  let rec parse_scope_aux tokens p =
    match tokens with
      | [] | {token_type = EOF; _}::_ -> failwith ("Unexpected EOF, missing }")
      | tok::next when tok.token_type == RIGHT_BRACE ->
          if p ==0 then
            (match next with
              | {token_type = SEMICOLON;_}::nnext -> [], nnext
              | _-> [], next
            )
          else
            let st, next = (parse_scope_aux next (p-1) ) in
            tok::st, next
      | last::right_brace::next
        when last.token_type != SEMICOLON
        && right_brace.token_type == RIGHT_BRACE ->
        parse_scope_aux (last::(semi_col_token ())::right_brace::next) p
      | tok::next when tok.token_type == LEFT_BRACE ->
      let st, next = (parse_scope_aux next (p+1)) in tok::st, next
      | tok::next->
      let st, next = (parse_scope_aux next p) in tok::st, next
  and semi_col_token () = {token_type=SEMICOLON; lexeme=";";literal=None;line=0}
  in parse_scope_aux tokens 0

let parse_paren tokens =
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
        && right_PAREN.token_type == RIGHT_PAREN ->
        parse_paren_aux (last::(semi_col_token ())::right_PAREN::next) p
      | tok::next when tok.token_type == LEFT_BRACE ->
      let st, next = (parse_paren_aux next (p+1)) in tok::st, next
      | tok::next->
      let st, next = (parse_paren_aux next p) in tok::st, next
  and semi_col_token () = {token_type=SEMICOLON; lexeme=";";literal=None;line=0}
  in parse_paren_aux tokens 0


let rec parse_program tokens =
  parse_declaration_sequence tokens

and parse_declaration_sequence tokens =
  let rec loop tokens =
  let ast, next = parse_declaration tokens in
    match next with
    | [] | { token_type = EOF; _}::_-> [ast]
    | _ -> ast :: (loop next)

  in StatementSequence (loop tokens)

and parse_declaration tokens =
  match tokens with
    (*Simple Statements/declarations*)
    | [] -> EMPTY, []
    | { token_type = VAR; _}::var_decl ->
      let statement_tokens, next = parse_semicol var_decl [] in
      parse_variable_declaration statement_tokens, next
    | { token_type = IDENTIFIER; _}::{ token_type = EQUAL; _}::_ ->
      let statement_tokens, next = parse_semicol tokens [] in
      parse_variable_mutation statement_tokens, next
    | { token_type = EOF; _}::_ -> EMPTY, []
    | { token_type = PRINT; _}::q ->
      let statement_tokens, next = parse_semicol q [] in
      let expr, _ = parse_expression statement_tokens in
      (PrintStatement expr), next
    | { token_type = IF; _}::_->
      parse_if_statement tokens
    | { token_type = WHILE; _}::_->
      parse_while_statement tokens
    | { token_type = FOR; _}::next ->
      parse_for_statement next

    (*Scope*)
    | {token_type =  LEFT_BRACE;_}::tx ->
      (
      let scope_tokens, next = parse_scope tx in
      let decl_seq = parse_declaration_sequence scope_tokens in
      (**print_endline ("======SCOPE AST=====\n" ^ show_ast decl_seq);**)
      match decl_seq with
        | StatementSequence l_decl when List.length l_decl > 0 ->
          Block l_decl, next
        | _-> failwith("[BlockParsingError] Expected at least one statement")
      )

    | _ ->  let statement_tokens, next = parse_semicol tokens [] in
            let expr, _ = parse_expression statement_tokens in
            (ExpressionStatement expr), next

and parse_variable_declaration tokens =
  match tokens with
    | identifier::{token_type = EQUAL; _}::decl_tokens
        when identifier.token_type == IDENTIFIER ->
        let decl_t,_  = parse_expression decl_tokens in
          VariableDeclaration ({name=identifier.lexeme;value=decl_t})
    | _-> failwith("[VariableDeclarationError] Wrong variable declaration")

and parse_variable_mutation tokens =
  match tokens with
  | identifier::{token_type = EQUAL; _}::mut_tokens
      when identifier.token_type == IDENTIFIER ->
      let mut_t,_  = parse_expression mut_tokens in
        VariableMutation ({name=identifier.lexeme;value=mut_t})
  | _-> failwith("[VariableMutationError] Wrong variable mutation")

and parse_if_statement tokens =
        match tokens with
        | {token_type = IF; _}::t::if_tokens  when t.token_type == LEFT_PAREN ->
    let condition_expr, next = parse_expression (t::if_tokens) in
    let then_expr, next = parse_declaration next in
    let else_expr, next =
    (
      match next with
        | {token_type = ELSE; _}::else_tokens ->
          let else_expr, next = parse_declaration else_tokens in
            (Some else_expr), next
        |_ -> None, next
    )
      in IfStatement (
        {condition=condition_expr;
          then_branch=then_expr;
          else_branch=else_expr}
        ), next

  | _-> failwith "Expected if statement"

and parse_while_statement tokens =
  match tokens with
  | {token_type = WHILE; _}::t::while_tokens  when t.token_type == LEFT_PAREN ->
    let condition_expr, next = parse_expression (t::while_tokens) in
    let loop_expr, next = parse_declaration next in
       WhileStatement (
        {condition=condition_expr;
          loop=loop_expr}
        ), next

  | _-> failwith "Expected while statement"

and parse_for_statement tokens =
  match tokens with
  | {token_type = LEFT_PAREN; _}::next ->
    (
    let clauses_tokens, next = parse_paren next in
    let clauses = parse_declaration_sequence clauses_tokens in
    match clauses with
    | StatementSequence s ->
      (
        match s with
        | var_init::loop_condition::incr::[] ->
          let loop, next = parse_declaration next in

         ForStatement (
           {
             var_decl=var_init;
             loop_condition=loop_condition;
             incr=incr;
             loop=loop
           }
         ), next

        |_ -> failwith "Incorrect for loop clauses"
      )
    |_ -> failwith "Incorrect for loop clauses"
    )
  | _-> failwith "[For loop error] Expected clauses between parentheses"



let parse tokens =
  let ast, remaining = parse_expression tokens in
  match remaining with
    | [] | [{ token_type = EOF; _ }] -> ast
    | t :: _ -> failwith ("Unexpected trailing token: " ^ show_tokenType t.token_type)
