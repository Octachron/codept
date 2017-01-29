
let tool_name = "codept-client"
let version = 0.3
let stderr= Format.err_formatter
let std = Format.std_formatter

let io = Io.direct

let addr port = Unix.( ADDR_INET (inet_addr_loopback, port) )


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

let make_socket () = Unix.(socket PF_INET SOCK_STREAM 0)

let get_port socket =
  match Unix.getsockname socket with
  | Unix.ADDR_INET(_,port) -> port
  | ADDR_UNIX _  -> raise (Invalid_argument "no port for unix socket")

let launch_server () =
  let socket = make_socket () in
  let addr = addr 0 in
  Unix.bind socket addr;
  Unix.listen socket 10;
  let port = get_port socket in
  let _n =
    Unix.create_process "codept-server"
      [|"codept-server"; "-backport"; string_of_int port  |]
      Unix.stdin Unix.stdout Unix.stderr in
  let s, _ = Unix.accept socket in
  let chan = Unix.in_channel_of_descr s in
  let n : int = input_value chan in
  n

let port = ref (-1)
let arg = Arg.["-port", Int ( fun n -> port := n ) , "server port"]

let client socket =
  let query = Parse_arg.process version ~extra:arg Sys.argv in
  if !port < 0 then
    port := launch_server ();
  let addr = addr ! port in
  let connect = try_connect socket addr in
  let connect =
    connect ||
    retry_connect "Retry" 0.20 (Unix.time()) socket addr in
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
