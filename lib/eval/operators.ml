open Result

let to_str (a:result) =
  match a with
    | Value aa ->
    (match aa with
      | String_t s -> "\"" ^s ^"\""
      | Boolean_t b -> if b then "True" else "False"
      | Float_t x -> string_of_float x
      | NoneValue -> "None"
    )
    | EvaluationError s -> s
    | Nil -> "Nil"

let fn_not (b: type_t) =
  match b with
    | Boolean_t bb -> Value (Boolean_t (not bb))
    | NoneValue -> EvaluationError ("Wrong type, expected Bool, got None")
    | Float_t _ -> EvaluationError ("Wrong type, expected Bool, got Float")
    | String_t _ -> EvaluationError ("Wrong type, expected Bool, got String")

let fn_neg (b: type_t) =
  match b with
    | NoneValue -> EvaluationError ("Wrong type, expected Float, got None")
    | Boolean_t bb -> EvaluationError ("Wrong type, expected Float, got Bool")
    | Float_t x -> Value (Float_t (-.x))
    | String_t _ -> EvaluationError ("Wrong type, expected Float, got String")

let fn_add (a:type_t) (b:type_t) =
  match a, b with
    | Boolean_t _, _ |  _, Boolean_t _ -> EvaluationError "Can not perform add on type Bool"
    | Float_t x, Float_t y -> Value (Float_t (x+.y))
    | String_t s, String_t ss -> Value (String_t (s^ss))
    | _,_ -> EvaluationError "missmatch types"

let fn_sub (a:type_t) (b:type_t) =
  match a, b with
    | Float_t x, Float_t y -> Value (Float_t (x-.y))
    | _,_ -> EvaluationError "missmatch types"

let fn_mult (a:type_t) (b:type_t) =
  match a, b with
    | Float_t x, Float_t y -> Value (Float_t (x*.y))
    | _,_ -> EvaluationError "missmatch types"

let fn_div (a:type_t) (b:type_t) =
  match a, b with
    | Float_t x, Float_t y -> Value (Float_t (x/.y))
    | _,_ -> EvaluationError "missmatch types"

let fn_eq (a:type_t) (b:type_t) =
  print_endline (show_type_t a ^" == " ^show_type_t b);
  match a, b with
  | Float_t x, Float_t y ->    Value (Boolean_t (Float.equal x y))
  | String_t x, String_t y -> Value (Boolean_t (String.equal x y))
  | Boolean_t x, Boolean_t y -> Value (Boolean_t (x == y))
  | NoneValue, NoneValue -> Value (Boolean_t true)
  |_-> Value (Boolean_t false)



let fn_neq (a:type_t) (b:type_t) =
  Value (Boolean_t (a != b))

let fn_gt (a:type_t) (b:type_t)  =
  match a, b with
    | Boolean_t a, Boolean_t b -> Value (Boolean_t (a >b))
    | Float_t a, Float_t b -> Value (Boolean_t (a >b))
    | String_t a, String_t b -> Value (Boolean_t (a >b))
    | _ -> EvaluationError "missmatch types"

let fn_geq (a:type_t) (b:type_t)  =
  match a, b with
    | Boolean_t a, Boolean_t b -> Value (Boolean_t (a >= b))
    | Float_t a, Float_t b -> Value (Boolean_t (a >= b))
    | String_t a, String_t b -> Value (Boolean_t (a >= b))
    | _ -> EvaluationError "missmatch types"

let fn_lt (a:type_t) (b:type_t)  =
  match a, b with
    | Boolean_t a, Boolean_t b -> Value (Boolean_t (a < b))
    | Float_t a, Float_t b -> Value (Boolean_t (a < b))
    | String_t a, String_t b -> Value (Boolean_t (a < b))
    | _ -> EvaluationError "missmatch types"

let fn_leq (a:type_t) (b:type_t)  =
  match a, b with
    | Boolean_t a, Boolean_t b -> Value (Boolean_t (a <= b))
    | Float_t a, Float_t b -> Value (Boolean_t (a <= b))
    | String_t a, String_t b -> Value (Boolean_t (a <= b))
    | _ -> EvaluationError "missmatch types"

let fn_print (a:type_t) =
  print_endline (to_str (Value a)); Nil
