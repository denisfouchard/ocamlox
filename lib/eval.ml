open Parser
open Environment
type result = | EvaluationError of string  | Value of type_t | Nil

let print_result res =
  match res with
  |EvaluationError msg -> print_endline msg
| Value x -> print_endline (show_type_t x)
| Nil -> print_endline "Nil"

let to_str (a:result) =
  match a with
  |Value aa ->(
    match aa with
    |String_t s -> "\"" ^s ^"\""
    |Boolean_t b -> if b then "True" else "False"
    |Float_t x -> string_of_float x
    |NoneValue -> "None")
  | EvaluationError s -> s
  |Nil -> "Nil"

let fn_not (b: type_t) =
  match b with
  |Boolean_t bb -> Value (Boolean_t (not bb))
  |NoneValue -> EvaluationError ("Wrong type, expected Bool, got None")
  |Float_t _ -> EvaluationError ("Wrong type, expected Bool, got Float")
  |String_t _ -> EvaluationError ("Wrong type, expected Bool, got String")

let fn_neg (b: type_t) =
  match b with
  |NoneValue -> EvaluationError ("Wrong type, expected Float, got None")
  |Boolean_t bb -> EvaluationError ("Wrong type, expected Float, got Bool")
  |Float_t x -> Value (Float_t (-.x))
  |String_t _ -> EvaluationError ("Wrong type, expected Float, got String")

let fn_add (a:type_t) (b:type_t) =
  match a, b with
  |Boolean_t _, _ | _, Boolean_t _ -> EvaluationError "Can not perform add on type Bool"
  |Float_t x, Float_t y -> Value (Float_t (x+.y))
  |String_t s, String_t ss -> Value (String_t (s^ss))
  |_,_ -> EvaluationError "missmatch types"

let fn_sub (a:type_t) (b:type_t) =
  match a, b with
  |Float_t x, Float_t y -> Value (Float_t (x-.y))
  |_,_ -> EvaluationError "missmatch types"

let fn_mult (a:type_t) (b:type_t) =
  match a, b with
  |Float_t x, Float_t y -> Value (Float_t (x*.y))
  |_,_ -> EvaluationError "missmatch types"

let fn_div (a:type_t) (b:type_t) =
  match a, b with
  |Float_t x, Float_t y -> Value (Float_t (x/.y))
  |_,_ -> EvaluationError "missmatch types"

let fn_eq (a:type_t) (b:type_t) =
  Value (Boolean_t (a == b))

let fn_neq (a:type_t) (b:type_t) =
  Value (Boolean_t (a != b))

let fn_gt (a:type_t) (b:type_t)  =
  match a, b with
  |Boolean_t a, Boolean_t b -> Value (Boolean_t (a >b))
  |Float_t a, Float_t b -> Value (Boolean_t (a >b))
  |String_t a, String_t b -> Value (Boolean_t (a >b))
  |_ -> EvaluationError "missmatch types"

let fn_geq (a:type_t) (b:type_t)  =
  match a, b with
  |Boolean_t a, Boolean_t b -> Value (Boolean_t (a >= b))
  |Float_t a, Float_t b -> Value (Boolean_t (a >= b))
  |String_t a, String_t b -> Value (Boolean_t (a >= b))
  |_ -> EvaluationError "missmatch types"

let fn_lt (a:type_t) (b:type_t)  =
  match a, b with
  |Boolean_t a, Boolean_t b -> Value (Boolean_t (a < b))
  |Float_t a, Float_t b -> Value (Boolean_t (a < b))
  |String_t a, String_t b -> Value (Boolean_t (a < b))
  |_ -> EvaluationError "missmatch types"

let fn_leq (a:type_t) (b:type_t)  =
  match a, b with
  |Boolean_t a, Boolean_t b -> Value (Boolean_t (a <= b))
  |Float_t a, Float_t b -> Value (Boolean_t (a <= b))
  |String_t a, String_t b -> Value (Boolean_t (a <= b))
  |_ -> EvaluationError "missmatch types"

let fn_print (a:type_t) =
  print_endline (to_str (Value a)); Nil


let eval (t:ast) =
let rec eval_env (t:ast) env =
  match t with
  | EMPTY ->env, Value NoneValue
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

  (*Unary operators*)
  |NOT (expr) ->env, unary_operator env expr fn_not
  |NEG (expr) ->env, unary_operator env expr fn_neg

 (*Literals*)
 | NUMBER_VALUE x ->env, Value (Float_t x)
 | STRING_VALUE s ->env, Value (String_t s)
 | BOOL_TRUE ->env, Value (Boolean_t true)
 | BOOL_FALSE ->env, Value (Boolean_t false)
 | NIL_VALUE ->env, Value NoneValue

(*Statements*)
 | ExpressionStatement expr ->
    let env_, v = eval_env expr env  in print_result v; env, v
 | PrintStatement expr ->env, unary_operator env expr fn_print
 | StatementSequence l ->
   (match l with
     |[] -> env, Nil
     |expr::q -> let env, res = eval_env expr env in
    eval_env (StatementSequence q) env)

(*Variable*)
| VariableDeclaration ({name=n;value=var_t}) ->
  var_decl_operator env n var_t
| VariableAccess s ->env,
  (match Environment.get_var env s with
  | Some x -> Value x
  | None -> EvaluationError ("UnboundValueError : "^ s ^" not defined" ))
| VariableMutation ({name=n;value=var_t}) ->
  var_decl_operator env n var_t
|_ ->env, EvaluationError ("Not implemented : " ^ show_ast t)


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
    |Value right_vv -> op_fn left_vv right_vv)

and unary_operator env t op_fn =
  let _, v = eval_env t env in
  match v with
  | EvaluationError msg -> EvaluationError msg
  | Nil -> EvaluationError "expected value, got none"
  | Value vv -> op_fn vv

and var_decl_operator env (name:string) (t:ast) =
  let env_, v = eval_env t env in
  match v with
  | EvaluationError msg -> env, (EvaluationError msg)
  | Nil-> env, EvaluationError "can not define a variable with no value"
  | Value vv->
    print_endline ("Var(" ^ name ^ ") = " ^ (to_str v));
    (Environment.define env_ name vv), Nil

in let _, res = eval_env t (Environment.empty) in res
