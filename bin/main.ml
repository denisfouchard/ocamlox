open Arg
open Format
let usage_msg = "eval [<file>] [--expression <expr>]";;

let file = ref "";;
let expression = ref "";;
let anon_fun filename = file:= filename;;
let speclist = [("--expression", Arg.Set_string expression, "inline expression to evaluate")];;
let () = Arg.parse speclist anon_fun usage_msg;;

let read_whole_file filename =
    (* open_in_bin works correctly on Unix and Windows *)
    let ch = open_in_bin filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s;;

let run_source(source: string) = print_endline source;;
let launch_repl () = print_endline "Lox REPL v0.0\n>>>\nComing soon...";
;;

let format_error (line:int) (where:string) (message:string) =
  Format.printf "@[<1>Error at line %d : %s %s @]"  line message where;;



let run () =
  if String.length !expression == 0
  then

    if String.length !file == 0
    then launch_repl ()
    else let file_source_code = read_whole_file !file in
      run_source file_source_code
  else (run_source !expression)
;;

run () ;;
