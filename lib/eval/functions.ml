open Parsing.Ast
open Environment
open Result

let rec bind_args
  (fn_args: ast list)
  (call_params:result list)
  env =
  if List.length call_params != List.length (fn_args)
  then env, EvaluationError (
  "Error: Arguments number mismatch: expected "
  ^ (string_of_int (List.length (fn_args)))
  ^ " , got "
  ^ (string_of_int (List.length call_params))
  )
  else (
    match fn_args, call_params with
    | [], [] -> env, Nil
    | (VariableAccess var_name)::next_args, (Value x)::next_call_p ->
      let new_env = Environment.define env var_name x in
     bind_args next_args next_call_p new_env
    | (VariableAccess var_name)::next_args, _ ->
      env, EvaluationError ("Invalid parameter for arg" ^ var_name)
    | _, _ ->
      env, EvaluationError ("Invalid function definition")
  )


