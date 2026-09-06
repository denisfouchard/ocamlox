open Parser

type type_t = | Boolean_t of bool | Float_t of float | String_t of string [@@deriving show]
type 'a result = | EvaluationError of string  | Value of type_t | NoneValue

let print_result res =
  match res with
  |EvaluationError msg -> print_endline msg
| Value x -> print_endline (show_type_t x)
| NoneValue -> print_endline "NoneValue"

let fn_not (b: type_t) =
  match b with
  |Boolean_t bb -> Value (Boolean_t (not bb))
  |Float_t _ -> EvaluationError ("Wrong type, expected Bool, got Float")
  |String_t _ -> EvaluationError ("Wrong type, expected Bool, got String")

let fn_neg (b: type_t) =
  match b with
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
  let to_str (a:type_t) =
    match a with
    |String_t s -> s
    |Boolean_t b -> if b then "True" else "False"
    |Float_t x -> string_of_float x
  in
  print_endline (to_str a); NoneValue


let rec eval (t:ast) =
  match t with
  | EMPTY -> NoneValue
  (*Binary operators*)
  | IS_EQAL (expr_left, expr_right) -> binary_operator expr_left expr_right fn_eq
  | IS_NEQ (expr_left, expr_right) -> binary_operator expr_left expr_right fn_neq
  | LT (expr_left, expr_right) -> binary_operator expr_left expr_right fn_lt
  | LEQ (expr_left, expr_right) -> binary_operator expr_left expr_right fn_leq
  | GT (expr_left, expr_right) -> binary_operator expr_left expr_right fn_gt
  | GEQ (expr_left, expr_right) -> binary_operator expr_left expr_right fn_geq
  | ADD (expr_left, expr_right) -> binary_operator expr_left expr_right fn_add
  | SUB (expr_left, expr_right) -> binary_operator expr_left expr_right fn_sub
  | MULT (expr_left, expr_right) -> binary_operator expr_left expr_right fn_mult
  | DIV (expr_left, expr_right) -> binary_operator expr_left expr_right fn_div

  (*Unary operators*)
  |NOT (expr) -> unary_operator expr fn_not
  |NEG (expr) -> unary_operator expr fn_neg

 (*Literals*)
 | NUMBER_VALUE x -> Value (Float_t x)
 | STRING_VALUE s -> Value (String_t s)
 | BOOL_TRUE -> Value (Boolean_t true)
 | BOOL_FALSE -> Value (Boolean_t false)
 | NIL_VALUE -> NoneValue

(*Statements*)
 | ExpressionStatement expr -> NoneValue
 | PrintStatement expr -> unary_operator expr fn_print
 | StatementSequence l ->
   (match l with
   |[] -> NoneValue
   |expr::q -> let _ = eval expr in  eval (StatementSequence q))
 |_ -> EvaluationError ("Not implemented : " ^ show_ast t)

and binary_operator left_t right_t op_fn =
  let left_v = eval left_t in
  match left_v with
  |EvaluationError msg -> EvaluationError msg
  |NoneValue -> EvaluationError "expected value, got none"
  |Value left_vv -> let right_v = eval right_t in
    (match right_v with
    |EvaluationError msg -> EvaluationError msg
    |NoneValue -> EvaluationError "expected value, got none"
    |Value right_vv -> op_fn left_vv right_vv)

and unary_operator t op_fn =
  let v = eval t in
  match v with
  | EvaluationError msg -> EvaluationError msg
  |NoneValue -> EvaluationError "expected value, got none"
  | Value vv -> op_fn vv
