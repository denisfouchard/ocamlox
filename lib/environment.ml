
type env_type = Float_env of float | Bool_env of bool | String_env of string

module Environment = struct

  type t = Hashtbl of string * env_type

  type 'a variable_access =
    | Variable of 'a
    | UnboundVariableError of string

  let new_env () = Hashtbl.create 100

  let define env name value =
   Hashtbl.add env name value


  let get_var env name =
    let res = Hashtbl.find_opt env name in
    match res with
    | None -> UnboundVariableError (name ^" does not exist.")
    | Some v -> Variable v
end
