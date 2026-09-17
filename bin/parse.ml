open Arg
open Format
open Ocamlox.Scanner
open Ocamlox.Eval
open Ocamlox.Parsing.Main
open Ocamlox.Eval.Result
open Ocamlox.Eval.Main
open Ocamlox.Parsing.Ast
let usage_msg = "ocamlox [<file>] [--expression <expr>]"



let file = ref ""
let expression = ref ""
let anon_fun filename = file := filename
let speclist = [ ("--expression", Arg.Set_string expression, "inline expression to evaluate") ]

let read_whole_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s


let run_source (source : string) =
  let ctx = init_scanner source in
  let result = scan_source ctx in
  let token_list = List.rev result.tokens in
    let t = parse token_list in
    print_endline (show_ast t)




let launch_repl () =
  Ocamlox.Repl.launch_repl ()

let format_error (line : int) (where : string) (message : string) =
  Format.printf "@[<1>Error at line %d : %s %s @]" line message where

let run () =
  Arg.parse speclist anon_fun usage_msg;
  if String.length !expression == 0 then
    if String.length !file == 0 then launch_repl ()
    else
    (print_endline ("Executing file:" ^ !file);
      let file_source_code = read_whole_file !file in
      run_source file_source_code)
    else (print_endline ("Eval expression: " ^ !expression); run_source !expression)

let () = run ()
