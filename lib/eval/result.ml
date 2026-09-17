type type_t =
  | Boolean_t of bool
  | Float_t of float
  | String_t of string
  | NoneValue
[@@deriving show]

type result =
  | EvaluationError of string
  | Value of type_t
  | Nil
[@@deriving show]
