open Parsing.Ast
type type_t =
  | Boolean_t of bool
  | Float_t of float
  | String_t of string
  | Callable of {args:ast list; body:ast}
  | NoneValue
[@@deriving show]

type result =
  | EvaluationError of string
  | Value of type_t
  | Nil
  | Return of type_t
[@@deriving show]
