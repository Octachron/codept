
let tool_name = "codept-client"
let version = 0.3
let stderr= Format.err_formatter
let std = Format.std_formatter

let io = Analysis.direct_io

let addr = Unix.( ADDR_UNIX "codept_test_3" )

let rec retry_connect timeout time sock addr =
  try Unix.connect sock addr with
  | Unix.(Unix_error(ECONNREFUSED,_,_)) ->
    let time' = Unix.time () in
    if time' -. time > timeout then
      (Printf.printf "Unable to connect to server\n";
       exit 2
      )
    else
      retry_connect timeout time sock addr


let make_socket () = Unix.(socket PF_UNIX SOCK_STREAM 0)

let client socket =
  let query = Parse_arg.process version Sys.argv in
  retry_connect 1. (Unix.time ()) socket addr;
  let ch = Unix.out_channel_of_descr socket in
  let inchan = Unix.in_channel_of_descr socket in
  output_value ch query;
  flush ch;
  let rec read () =
    match input_line inchan with
    | exception End_of_file -> ()
    | s -> print_endline s; read () in
  read ()


;; client @@ make_socket ()
  (*
  let task = io.findlib query.task query.findlib in
  List.iter (Parse_arg.eval_single query.params query.task) query.action.singles;
  if not (query.action.modes = [] && query.action.makefiles = [] ) then
    let analyzed = Analysis.main io query.params.analyzer task in
    List.iter (Parse_arg.iter_mode query.params analyzed) query.action.modes;
    List.iter (Parse_arg.iter_makefile query.params analyzed)
      query.action.makefiles
*)
