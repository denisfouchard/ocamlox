open Scanner
open Ast
open Expression

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
    | { token_type = EOF; _}::_ -> EMPTY, []

    (*variables*)
    | { token_type = VAR; _}::var_decl ->
      let statement_tokens, next = parse_semicol var_decl [] in
      parse_variable_declaration statement_tokens, next
    | { token_type = IDENTIFIER; _}::{ token_type = EQUAL; _}::_ ->
      let statement_tokens, next = parse_semicol tokens [] in
      parse_variable_mutation statement_tokens, next

    (**Functions**)
    | { token_type = PRINT; _}::q ->
      let statement_tokens, next = parse_semicol q [] in
      let expr, _ = parse_expression statement_tokens in
      (PrintStatement expr), next
    | { token_type = FUN; _}::fun_tokens ->
      parse_function_declaration fun_tokens

    (*Control Flow*)
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
    let clauses_tokens, next = parse_paren next true in
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

and parse_function_declaration tokens =
  match tokens with
  | tok::par::next ->
      if tok.token_type != IDENTIFIER
      then failwith "Expected function name"
      else (
        if par.token_type != LEFT_PAREN then failwith "Incorrect function declaration syntax"
        else
        let fn_name = tok.lexeme in
        let arg_tokens, next = parse_paren next false in
        let args = parse_args arg_tokens in
        let body_t, next = parse_declaration next in
        (
          match body_t with
          | Block l ->
            FunctionDeclaration (
              {
                name=fn_name;
                arguments=args;
                body=body_t
              }
            ), next
          | _ -> failwith "Incorrect function body"
        )
      )
      | _-> failwith "Incorrect function declaration syntax"
