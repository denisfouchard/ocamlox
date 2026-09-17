open Parsing
open Environment
open Result
open Parsing.Ast
open Operators

(**Main Eval**)
let eval (t:ast) =
  let rec eval_env (t:ast) env =
    match t with
      | EMPTY -> env, Value NoneValue
      (*Binary operators*)
      | IS_EQAL (expr_left, expr_right) ->env,
          binary_operator env expr_left expr_right fn_eq
      | IS_NEQ (expr_left, expr_right) ->env,
          binary_operator env expr_left expr_right fn_neq
      | LT (expr_left, expr_right) ->env,
          binary_operator env expr_left expr_right fn_lt
      | LEQ (expr_left, expr_right) ->env,
          binary_operator env expr_left expr_right fn_leq
      | GT (expr_left, expr_right) ->env,
          binary_operator env expr_left expr_right fn_gt
      | GEQ (expr_left, expr_right) ->env,
          binary_operator env expr_left expr_right fn_geq
      | ADD (expr_left, expr_right) ->env,
          binary_operator env expr_left expr_right fn_add
      | SUB (expr_left, expr_right) ->env,
          binary_operator env expr_left expr_right fn_sub
      | MULT (expr_left, expr_right) ->env,
          binary_operator env expr_left expr_right fn_mult
      | DIV (expr_left, expr_right) ->env,
          binary_operator env expr_left expr_right fn_div

      (*Logical operators*)
      | OR (left, right) -> env,
        or_operator env left right
      | AND (left, right) -> env,
        and_operator env left right

      (*Unary operators*)
      | NOT (expr) ->env, unary_operator env expr fn_not
      | NEG (expr) ->env, unary_operator env expr fn_neg

      (*Literals*)
      | NUMBER_VALUE x ->env, Value (Float_t x)
      | STRING_VALUE s ->env, Value (String_t s)
      | BOOL_TRUE ->env, Value (Boolean_t true)
      | BOOL_FALSE ->env, Value (Boolean_t false)
      | NIL_VALUE ->env, Value NoneValue

      (*Statements*)
      | ExpressionStatement expr -> eval_env expr env
      | PrintStatement expr ->env, unary_operator env expr fn_print
      | StatementSequence l ->
        (match l with
          |[] -> env, Nil
          |expr::q -> let env, res = eval_env expr env in
            match res with
            | EvaluationError msg -> env, res (* If error is caught, early stopping*)
            |_->eval_env (StatementSequence q) env
        )

      (*Variable*)
      | VariableDeclaration ({name=n;value=var_t}) ->
        var_decl_operator env n var_t
      | VariableAccess s ->env,
        (match Environment.get_var env s with
          | Some x -> Value x
          | None -> EvaluationError ("UnboundValueError : "^ s ^" not defined" )
        )
      | VariableMutation ({name=n;value=var_t}) ->
        var_mut_operator env n var_t


      | Block l ->
        let local_env = (Environment.create_local_scope env) in
        (match l with
          |[] -> (Environment.delete_local_scope local_env), Nil
          |expr::q ->
            let local_env', res = eval_env expr local_env in
              eval_env (StatementSequence q) local_env'
        )

      | IfStatement ({condition=c; then_branch=tb; else_branch=eb}) ->
          eval_if_statement c tb eb env
      | WhileStatement({condition=c; loop=l}) ->
          eval_while_statement c l env

      | ForStatement (
            {
              var_decl=var_init;
              loop_condition=loop_condition;
              incr=incr;
              loop=loop
            }
          ) ->
          let local_env = (Environment.create_local_scope env) in
          let local_env, res =
            eval_for_statement var_init loop_condition incr loop local_env
          in (Environment.delete_local_scope local_env), res



      (**Not implemented**)
      | _ ->env, EvaluationError ("Not implemented : " ^ show_ast t)

  and binary_operator env left_t right_t op_fn =
    let _, left_v = eval_env left_t env  in
    match left_v with
      |EvaluationError msg -> EvaluationError msg
      |Nil -> EvaluationError "expected value, got none"
      |Value left_vv ->
        let _, right_v = eval_env right_t env in
        (match right_v with
          |EvaluationError msg -> EvaluationError msg
          |Nil -> EvaluationError "expected value, got none"
          |Value right_vv -> op_fn left_vv right_vv
        )

  and unary_operator env t op_fn =
    let _, v = eval_env t env in
    match v with
      | EvaluationError msg -> EvaluationError msg
      | Nil -> EvaluationError "expected value, got none"
      | Value vv -> op_fn vv

  and or_operator env left right =
    let env, v = eval_env left env in
    match v with
    | EvaluationError msg -> EvaluationError msg
    | Nil -> EvaluationError "expected value, got none"
    | Value (Boolean_t true) -> v
    | Value (Boolean_t false) ->
      (
      let _, v_right = eval_env right env in
      match v_right with
      | EvaluationError msg -> EvaluationError msg
      | Nil -> EvaluationError "expected value, got none"
      | Value (Boolean_t x) -> v_right
      | Value t ->EvaluationError ("Expected expression of type bool, got " ^ show_type_type t)
      )
    | Value t -> EvaluationError ("Expected expression of type bool, got " ^ show_type_type t)

  and and_operator env left right =
    let env, v = eval_env left env in
    match v with
    | EvaluationError msg -> EvaluationError msg
    | Nil -> EvaluationError "expected value, got none"
    | Value (Boolean_t false) -> v
    | Value (Boolean_t true) ->
      (
      let _, v_right = eval_env right env in
      match v_right with
      | EvaluationError msg -> EvaluationError msg
      | Nil -> EvaluationError "expected value, got none"
      | Value (Boolean_t x) -> v_right
      | Value t ->EvaluationError ("Expected expression of type bool, got " ^ show_type_type t)
      )
    | Value t -> EvaluationError ("Expected expression of type bool, got " ^ show_type_type t)

  and var_mut_operator env (name:string) (t:ast) =
    let env_, v = eval_env t env in
    match v with
      | EvaluationError msg ->  env, (EvaluationError msg)
      | Nil->env, EvaluationError "can not define a variable with no value"
      | Value vv->
        print_endline ("Var(" ^ name ^ ") <- " ^ (to_str v));
        let new_env = Environment.mutate env_ name vv in
        (match new_env with
          |None -> env, EvaluationError ("UnboundValueError: " ^ name ^ " is not defined.")
          |Some new_env' -> new_env', Nil
        )

  and var_decl_operator env (name:string) (t:ast) =
    let env_, v = eval_env t env in
    match v with
      | EvaluationError msg -> env, (EvaluationError msg)
      | Nil-> env, EvaluationError "can not define a variable with no value"
      | Value vv->
        print_endline ("Var(" ^ name ^ ") = " ^ (to_str v));
        (Environment.define env_ name vv), Nil


  and eval_if_statement (condition:ast) (then_branch:ast) (else_branch:ast option) env =
    let env, condition_res = eval_env condition env in
    print_endline (show_result condition_res);
    let condition_res =
      (
      match condition_res with
      | Value (Float_t x) -> Value (Boolean_t (not (Float.equal x 0.)))
      |_ -> condition_res
      )
    in
    match condition_res with
    | EvaluationError msg -> env, condition_res
    | Nil -> env, EvaluationError ("Error: expected value inside conditional if, got Nil")
    | Value (Boolean_t b) ->
      if b then (eval_env then_branch env)
      else
      (
      match else_branch with
      | None -> env, Nil
      | Some else_b -> (eval_env else_b env)
      )

    | Value v -> env, EvaluationError ("Error: expected condition of type bool, got " ^ show_type_type v)

  and eval_while_statement (condition:ast) (loop:ast)  env =
    let env, condition_res = eval_env condition env in
    print_endline (show_result condition_res);
    let condition_res =
      (
      match condition_res with
      | Value (Float_t x) -> Value (Boolean_t (not (Float.equal x 0.)))
      |_ -> condition_res
      )
    in
    match condition_res with
    | EvaluationError msg -> env, condition_res
    | Nil -> env, EvaluationError ("Error: expected value inside conditional while, got Nil")
    | Value (Boolean_t b) ->
      if b then
        let env, _ = (eval_env loop env)
        in eval_while_statement condition loop env
        else env, Nil


    | Value v -> env, EvaluationError ("Error: expected condition of type bool, got " ^ show_type_type v)

  and eval_for_statement var_init loop_condition incr loop env =
    let var_init_env, _ = eval_env var_init env in
    let loop_incr =
    (
    match loop with
    | Block l -> Block (l @ [incr])
    | _ -> failwith ("Error: Incorrect format for loop execution")
    ) in
    eval_while_statement loop_condition loop_incr var_init_env


  in let _, res = eval_env t (Environment.empty) in res
