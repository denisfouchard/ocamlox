module VarTable = Map.Make(String);;

type type_t = | Boolean_t of bool | Float_t of float | String_t of string | NoneValue [@@deriving show]

module Environment = struct

  type t =
    |Global of type_t VarTable.t
    |Scope of {outside:t; local:t}

  let empty = Global VarTable.empty

  let rec define (env: t) (name:string) (value:type_t) =
    match env with
    | Global env_t ->Global (VarTable.add  name value env_t)
    | Scope {outside=env_out;local=env_t} ->
      Scope {outside=env_out;local=(define env_t name value)}

  let rec mutate (env:t) (name:string) (new_value:type_t) =
    match env with
    | Global env_t ->
      (match VarTable.find_opt name env_t with
      |None -> None
      |Some res -> Some (Global (VarTable.add name new_value env_t))
      )
    | Scope {outside=env_out;local=env_t} ->
      let mutate_in_local = mutate env_t name new_value in
      (match mutate_in_local with
        | None -> let try_outer_scope = mutate env_out name new_value in
          (match try_outer_scope with
          |None -> None
          |Some new_env -> Some (Scope {outside=new_env;local=env_t})
          )
        |Some new_env ->  Some (Scope {outside=env_out;local=new_env})
      )


  let rec get_var (env:t) (name:string) =
    match env with
    | Global env_t -> VarTable.find_opt name env_t
    | Scope {outside=env_out;local=env_t} ->
      let find_in_local = get_var env_t name in
        match find_in_local with
        | None -> get_var env_out name
        | Some res -> Some res


  let create_local_scope (env:t) =
    Scope {outside=env;local=empty}

  let rec delete_local_scope (env:t) =
    match env with
    | Global _ -> env
    | Scope {outside=out;local=(Global _)} -> out
    | Scope {outside=out;local=loc} ->
      Scope {outside=out;local=(delete_local_scope loc)}
end
