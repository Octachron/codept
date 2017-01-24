
let tool_name = "codept-client"
let version = 0.3
let stderr= Format.err_formatter
let std = Format.std_formatter

let io = Analysis.direct_io

let addr = Unix.( ADDR_UNIX "/tmp/codept" )


let try_connect sock addr =
  try Unix.connect sock addr; true with
  | Unix.(Unix_error((ECONNREFUSED|ENOENT),_,_)) -> false

let rec retry_connect msg timeout time sock addr =
  try Unix.connect sock addr; true with
  | Unix.(Unix_error((ECONNREFUSED|ENOENT),_,_)) ->
    let time' = Unix.time () in
    if time' -. time > timeout then
      begin
        Printf.printf "%s: unable to connect to server\n" msg;
        false
      end
    else
      retry_connect msg timeout time sock addr


let autostart = false
let make_socket () = Unix.(socket PF_UNIX SOCK_STREAM 0)

let launch_server () =
  ignore @@
  Unix.create_process "codept-server" [|"codept-server"|]
    Unix.stdin Unix.stdout Unix.stderr


let client socket =
  let query = Parse_arg.process version Sys.argv in
  let connect = try_connect socket addr in
  if connect  && autostart then
    begin
      launch_server ();
      if not @@ retry_connect "Retrying to connect" 0.020 (Unix.time ())
          socket addr then
        exit 2
    end;
  if connect then
    let ch = Unix.out_channel_of_descr socket in
    let inchan = Unix.in_channel_of_descr socket in
    output_value ch query;
    flush ch;
    let rec read () =
      match input_line inchan with
      | exception End_of_file -> ()
      | s -> print_endline s; read () in
    read ()
  else
    exit 2

;; client @@ make_socket ()
