(** Codept server *)
open Params
let tool_name = "codept-server"
let version = 0.3
let stderr= Format.err_formatter
let std = Format.std_formatter

let prefix = "_build"
let cache = Cache.Shared.make Cache.empty

module Sh = Cache.Shared

let sign filename =
  let cached = Sh.get cache in
  match Name.Map.find_opt filename cached.signatures with
  | Some _ as s -> s
  | None ->
    let s = Io.read_sigfile filename in
    begin match s with
      | None -> ()
      | Some s -> Sh.map (fun (cached:Cache.t) ->
        let map = Name.Map.add filename s cached.signatures in
        { cached with signatures = map }
        ) cache
    end;
    s

let m2l polycy k filename =
  let cached = Sh.get cache in
  let filename' = filename.Namespaced.name in
  match Name.Map.find_opt filename' cached.m2l with
  | Some u -> u
  | None ->
    let u = Unit.read_file polycy k filename in
    Sh.map (fun (cached:Cache.t) ->
        let map = Name.Map.add filename' u cached.m2l in
        { cached with m2l = map }
      ) cache;
    u

let findlib task query =
  let cached = Sh.get cache in
  match Cache.Findmap.find query cached.findlib with
  | (expand, register) -> register (); expand task
  | exception Not_found ->
    let result = Findlib.process query in
    let add_ppx ppx =
      let first_ppx = Compenv.first_ppx in
      first_ppx := ppx :: !first_ppx in
    let pp () = Clflags.preprocessor := result.pp in
    let register () =
      List.iter add_ppx result.ppxs; pp () in
    let expand (task:Common.task) =
      { task with libs =  result.libs @ task.libs } in
    Sh.map (fun (cached:Cache.t) ->
        let map = Cache.Findmap.add query (expand,register) cached.findlib in
        { cached with findlib = map }
      ) cache;
    register (); expand task

let io = {
  Io.reader = {
    Io.sign;
    m2l;
    env = Module.Def.empty;
    findlib;
  };
  writer = {
    m2l = begin
      fun (kind,filename) ppf m2l ->
        Sh.map (fun (cached:Cache.t) ->
            let src = Paths.P.local filename in
            let path = Namespaced.make @@ Paths.P.module_name src in
            let u: Unit.s =
              { precision = Exact; code = m2l;
                path; src; kind = kind.kind } in
            let m2l = Name.Map.add filename u cached.m2l in
            { cached with m2l }
          ) cache;
        Io.direct.writer.m2l (kind,filename) ppf m2l
    end;
    sign = begin
      fun filename ppf sign ->
        Sh.map (fun (cached:Cache.t) ->
            let signatures = Name.Map.add filename sign cached.signatures in
            { cached with signatures }
          ) cache;
        Io.direct.writer.sign filename ppf sign
    end;
  }
}


let addr = Unix.( ADDR_INET(inet_addr_loopback, 0) )
let socket = Unix.( socket PF_INET SOCK_STREAM 0)

(*
let () = Unix.setsockopt socket Unix.SO_REUSEADDR true
*)

let answer f where =
  let ch = Unix.in_channel_of_descr where in
  let out = Unix.out_channel_of_descr where in
  let query: Args.query = input_value ch in
  let fmt = Format.formatter_of_out_channel out in
  f fmt query;
  Format.fprintf fmt "@?";
  flush out;
  Unix.close where


let process out (query:Args.query) =
  let task = io.reader.findlib query.task query.findlib in
  List.iter (Args.eval_single out io.writer query.params query.task)
    query.action.singles;
  if not (query.action.modes = [] && query.action.makefiles = [] ) then
    let analyzed = Analysis.main io.reader query.params.analyzer task in
    List.iter (Args.iter_mode out io.writer query.params analyzed)
      query.action.modes;
    List.iter (Args.iter_makefile out query.params analyzed)
      query.action.makefiles


let rec server () =
  Unix.listen socket 10;
  match Unix.select [socket] [] [] 30. with
  | [_], _ , _  ->
    let client, _addr = Unix.accept socket in
    let _t = Thread.create (answer process) client in
    server ()
  | _ ->
    Unix.close socket

let usage_msg = "Codept server process"

let port = ref ~-1

let args = ["-backport", Arg.Int (fun n -> port := n),
            "port to send back binding port" ]

let get_port socket =
  match Unix.getsockname socket with
  | ADDR_INET(_,port) -> Some port
  | ADDR_UNIX _  -> None

let client_socket port =
  let addr = Unix.( ADDR_INET(inet_addr_loopback, port) ) in
  let socket = Unix.( socket PF_INET SOCK_STREAM 0) in
  Unix.connect socket addr;
  socket


let () =
  Arg.parse args ignore usage_msg;
  Unix.bind socket addr;
  if !port> 0 then
    begin
      let client_socket = client_socket !port in
      let out = Unix.out_channel_of_descr client_socket in
      Option.iter (output_value out) (get_port socket);
      flush out
    end
  else
    Option.iter (Printf.printf "Binding to %d\n") (get_port socket);
  flush stdout;
  Unix.listen socket 10;
  Sys.chdir prefix;
  server ()
