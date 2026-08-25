

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
  |IDENTIFIER
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
;;

type literal_type = STRING_LITERAL of string | NUMBER_LITERAL of float;;
type token = {
  token_type:tokenType;
    lexeme:string;
    literal:literal_type option;
    line:int
};;

type scanner_ctx = {
  source: string; start:int; current:int; line:int; tokens: token list
};;

let is_at_end (ctx:scanner_ctx) = ctx.current >= String.length ctx.source;;

let add_token_aux token_type lexeme literal line ctx =
(* Append at the beggining !!!!*)
  let new_tokens = {
    token_type; lexeme; literal; line;
  } :: ctx.tokens in
  {source=ctx.source;start=ctx.current+1; current=ctx.current;line=ctx.line;tokens=new_tokens}

;;



let current_char (ctx: scanner_ctx) : char option =
  if is_at_end ctx then None else Some (String.get ctx.source ctx.current);;

let advance (ctx:scanner_ctx) =
  {
    ctx with current = ctx.current +1
  }
;;

let newline (ctx:scanner_ctx) =
  {
    ctx with line = ctx.line +1
  };;

let add_token token_type literal ctx =
  let lexeme = match token_type with
    | EOF -> ""
    |_-> String.sub ctx.source ctx.start ctx.current in
  add_token_aux token_type lexeme literal ctx.line ctx
;;

let is_next c ctx = match (current_char (advance ctx)) with
  | None -> false
| Some x -> c == x;;

let rec skip_line ctx =
  match current_char ctx with
  | None -> {ctx with start = ctx.current +1}
  | Some '\n' -> {(advance (newline ctx)) with start = ctx.current +1}
  | Some c -> skip_line (advance ctx);;

let scan_string (ctx:scanner_ctx) =
  let rec scan_string_acc ctx acc =
    let c = current_char ctx in
    match c with
    | None | Some '\n'-> failwith "Incorrect formated string. Expected end of string"
    | Some char ->
      if char == '"'
          then
            let literal_token = Some (STRING_LITERAL acc)
            in (add_token STRING literal_token ctx) else
          scan_string_acc (advance ctx) ( acc ^ (String.make 1 char))
  in scan_string_acc ctx ""
    ;;

let scan_token (ctx:scanner_ctx) =
  let rec scan_second_token acc ctx =
    let ctx_= advance ctx in
      match current_char ctx_ with
        | None -> add_token EOF None ctx_
        | Some c ->
          match c with
          (*Meaningless*)
            |'\n' -> advance (newline ctx_)
            | '\r' | ' ' | '\t' -> advance ctx_
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
                      | SLASH -> skip_line ctx_
                      |_ -> scan_second_token SLASH ctx_)
            |'"' -> scan_string (advance ctx_)
            | _ -> failwith ("Unknown character " ^ (String.make 1 c) ^ " at line " ^ string_of_int ctx_.line)
    in scan_second_token BOF ctx;;



let rec scan_source ctx =
  print_endline ("Length of code : " ^string_of_int (String.length ctx.source));
  if is_at_end ctx then ctx
  else let new_ctx = scan_token ctx in scan_source new_ctx;;

let context = {source="(=)<><>({})//this is a zzcomment\n=\"pizi\"";start=0;current= -1;line=0;tokens=[]};;

List.rev (scan_source context).tokens;;
