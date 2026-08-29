type tokenType =
  | LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACE
  | RIGHT_BRACE
  | COMMA
  | DOT
  | MINUS
  | PLUS
  | SEMICOLON
  | SLASH
  | STAR

  | BANG
  | BANG_EQUAL
  | EQUAL
  | EQUAL_EQUAL
  | GREATER
  | GREATER_EQUAL
  | LESS
  | LESS_EQUAL

  (* Literals *)
  | IDENTIFIER
  | STRING
  | NUMBER

(* Keywords *)
  | AND
  | CLASS
  | ELSE
  | FALSE
  | FUN
  | FOR
  | IF
  | NIL
  | OR
  | PRINT
  | RETURN
  | SUPER
  | THIS
  | TRUE
  | VAR
  | WHILE
  | EOF
  | BOF

module Keywords = struct

  type keyword_map = (string, tokenType) Hashtbl.t
  let _table : keyword_map =
    let keywords = Hashtbl.create 15 in
        Hashtbl.add keywords "and"    AND;
        Hashtbl.add keywords "class"  CLASS;
        Hashtbl.add keywords "else"   ELSE;
        Hashtbl.add keywords "false"  FALSE;
        Hashtbl.add keywords "for"    FOR;
        Hashtbl.add keywords "fun"    FUN;
        Hashtbl.add keywords "if"     IF;
        Hashtbl.add keywords "nil"    NIL;
        Hashtbl.add keywords "or"     OR;
        Hashtbl.add keywords "print"  PRINT;
        Hashtbl.add keywords "return" RETURN;
        Hashtbl.add keywords "super"  SUPER;
        Hashtbl.add keywords "this"   THIS;
        Hashtbl.add keywords "true"   TRUE;
        Hashtbl.add keywords "var"    VAR;
        Hashtbl.add keywords "while"  WHILE;
        keywords

  let get_identifier (id_string:string) =
    if Hashtbl.mem _table id_string
    then Hashtbl.find _table id_string
    else IDENTIFIER

end

type literal_type = STRING_LITERAL of string | NUMBER_LITERAL of float
type token = {
  token_type:tokenType;
    lexeme:string;
    literal:literal_type option;
    line:int
}

type scanner_ctx = {
  source: string; start:int; current:int; line:int; tokens: token list
}

let init_scanner (source_code:string) =
  {
    source=source_code;start = 0; current = -1; line = 1; tokens = []
  }

let is_at_end (ctx:scanner_ctx) = ctx.current >= String.length ctx.source

let add_token_aux token_type lexeme literal line ctx =
(* Append at the beggining !!!!*)
  let new_tokens = {
    token_type; lexeme; literal; line;
  } :: ctx.tokens in

  {source=ctx.source;start=ctx.current+1; current=ctx.current;line=ctx.line;tokens=new_tokens}

let current_line (ctx:scanner_ctx) : string =
  string_of_int (ctx.line)

let current_char (ctx: scanner_ctx) : char option =
  if is_at_end ctx then None else Some (String.get ctx.source ctx.current)

let skip_char (ctx:scanner_ctx) = {ctx with start = ctx.current +1}

let advance (ctx:scanner_ctx) =
  {
    ctx with current = ctx.current +1
  }

let rollback (ctx:scanner_ctx) =
  {
    ctx with current = ctx.current -1
  }

let newline (ctx:scanner_ctx) =
  let ctx_ = {
    (ctx) with line = ctx.line +1
  } in {ctx_ with start = ctx.current +1}

let add_token token_type literal ctx =
  let lexeme = (
    match token_type with
    | EOF -> ""
    | _ -> (match literal with
      | None | Some (NUMBER_LITERAL _) ->
      (let len = ctx.current - ctx.start + 1  in
        if len < 0 then "" else String.sub ctx.source ctx.start len)
  | Some (STRING_LITERAL lit) -> lit))
  in
  add_token_aux token_type lexeme literal ctx.line ctx

let parse_int (ch:char) =
  let ch_code = int_of_char ch in
  if ch_code >= 48 && ch_code <=57 then (ch_code - 48) else -1

let is_alpha (ch:char) =
  (ch >= 'a' && ch <= 'z') ||
  (ch >= 'A' && ch <= 'Z') ||
  ch == '_'

let is_alphanumeric (ch:char) = is_alpha ch || parse_int ch >= 0

let is_next c ctx = match (current_char (advance ctx)) with
  | None -> false
| Some x -> c == x

let rec skip_line ctx =
  match current_char ctx with
  | None -> {ctx with start = ctx.current}
  | Some '\n' -> newline ctx
  | Some c -> skip_line (advance ctx)

let scan_string (ctx:scanner_ctx) =
  let rec scan_string_acc ctx acc =
    let c = current_char ctx in
    match c with
    | None | Some '\n'-> failwith ("[scanner.ml] Incorrect formated string at line" ^ (current_line ctx) ^". Expected end of string")
    | Some char ->
      if char == '"'
          then
            let literal_token = Some (STRING_LITERAL acc)
            in (add_token STRING literal_token ctx) else
          scan_string_acc (advance ctx) ( acc ^ (String.make 1 char))
  (* No rollback here because we need to skip the last quote character*)
  in scan_string_acc ctx ""

let scan_identifier (ctx:scanner_ctx)=
  let rec scan_ident_acc acc ctx =
    let c = current_char ctx in
    (match c with
    | Some chx when is_alphanumeric chx -> (scan_ident_acc (acc ^(String.make 1 chx)) (advance ctx) )
    | _ -> let ctx_ = rollback ctx in (
        match (Keywords.get_identifier acc) with
        | IDENTIFIER ->(add_token IDENTIFIER (Some (STRING_LITERAL acc)) ctx_)
        | tok_type ->(add_token tok_type None ctx_)
    ))
      in scan_ident_acc "" ctx


let parse_float partie_entiere decimale =
  let rec list_to_float l=
    match l with
    | [] -> 0.
    |h::t -> (float_of_int h) *. (10.** (float_of_int (List.length t))) +. (list_to_float t)
  in let pe_float = (list_to_float partie_entiere) in
  let dec_float = ((list_to_float decimale) *. (10.**float_of_int (-List.length decimale)))
  in pe_float +. dec_float


let scan_numerical (ctx:scanner_ctx) =
  let rec scan_num_acc (partie_entiere: int list) (decimale:int list) (flag:bool) (ctx:scanner_ctx)=
    let c = current_char ctx in
    match c with
    | None | Some '\n'-> (partie_entiere, decimale, ctx)
    | Some char -> let new_int = parse_int char in
                if  new_int < 0 && char != '.'
                    then (partie_entiere, decimale, ctx) else
                      if char == '.' then
                        let partie_entiere_, decimale_, ctx2 = (scan_num_acc partie_entiere decimale true (advance ctx))
                        in (partie_entiere, decimale_, ctx2)
                      else
                        let partie_entiere_, decimale_, ctx2 = (scan_num_acc partie_entiere decimale flag (advance ctx))
                        in if flag then (partie_entiere_, new_int::decimale_, ctx2)
                        else (new_int::partie_entiere_, decimale_, ctx2)

    in let pe, dec, ctx_ = (scan_num_acc [] [] false ctx) in

    let literal_token = Some (NUMBER_LITERAL (parse_float pe dec))
    in (add_token NUMBER literal_token (rollback ctx_))



let scan_token (ctx:scanner_ctx) =
  let rec scan_second_token acc ctx =
    let ctx_= advance ctx in
      match current_char ctx_ with
        | None -> add_token EOF None ctx_
        | Some c ->
          match c with
          (*Meaningless*)
            |'\n' -> (newline ctx_)
            | '\r' | ' ' | '\t' -> skip_char ctx_
            |'('-> add_token LEFT_PAREN None ctx_
            |')'-> add_token RIGHT_PAREN None ctx_
            |'{'-> add_token LEFT_BRACE None ctx_
            |'}'-> add_token RIGHT_BRACE None ctx_
            |','-> add_token COMMA None ctx_
            |'.'-> add_token DOT None ctx_
            |'-'-> add_token MINUS None ctx_
            |'+'-> add_token PLUS None ctx_
            |';'-> add_token SEMICOLON None ctx_
            |'*'-> add_token STAR None ctx_
            (* Double char operators *)
            |'!'-> if is_next '=' ctx_
                    then scan_second_token BANG ctx_
                    else add_token BANG None ctx_
            |'>' -> if is_next '=' ctx_
                      then scan_second_token GREATER ctx_
                      else add_token GREATER None ctx_
            |'<' ->  if is_next '=' ctx_
                      then scan_second_token LESS ctx_
                      else add_token LESS None ctx_
            |'=' -> (match acc with
                      | BANG -> add_token BANG_EQUAL None ctx_
                      | GREATER -> add_token GREATER_EQUAL None ctx_
                      | LESS -> add_token LESS_EQUAL None ctx_
                      | BOF -> add_token EQUAL None ctx_
                      |_ -> add_token EQUAL None (add_token acc None ctx_))
            (* Case of // for comment *)
            |'/' -> (match acc with
                      (*Comment*)
                      | SLASH -> (print_endline "Skip line"; skip_line ctx_)
                      |_ -> scan_second_token SLASH ctx_)
            |'"' -> scan_string (advance ctx_)
            | _ when parse_int c >= 0 -> scan_numerical  ctx_
            | _ when is_alpha c -> scan_identifier ctx_
            | _ -> failwith ("[scanner.ml] Unknown character " ^ (String.make 1 c) ^ " at line " ^ current_line ctx_ ^ ":" ^ string_of_int ctx_.current)
    in scan_second_token BOF ctx


let rec scan_source ctx =
  if is_at_end ctx then ctx
  else let new_ctx = scan_token ctx in scan_source new_ctx
