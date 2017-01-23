(** Codept server *)
open Params
let tool_name = "codept"
let version = 0.3
let stderr= Format.err_formatter
let std = Format.std_formatter

let cache = Cache.Shared.make Cache.empty

module Sh = Cache.Shared

let sign filename =
  let cached = Sh.get cache in
  match Name.Map.find_opt filename cached.signatures with
  | Some _ as s -> s
  | None ->
    let s = Analysis.read_sigfile filename in
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
  match Name.Map.find_opt filename cached.m2l with
  | Some u -> u
  | None ->
    let u = Unit.read_file polycy k filename in
    Sh.map (fun (cached:Cache.t) ->
        let map = Name.Map.add filename u cached.m2l in
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
  Analysis.sign;
  m2l;
  env = Module.Sig.empty;
  findlib;
}

let uaddr= "codept_test_3"

let addr = Unix.ADDR_UNIX uaddr
let socket = Unix.(socket PF_UNIX SOCK_STREAM 0)
let () = Unix.setsockopt socket Unix.SO_REUSEADDR true

let answer f where =
  let ch = Unix.in_channel_of_descr where in
  let out = Unix.out_channel_of_descr where in
  let query: Parse_arg.query = input_value ch in
  let fmt = Format.formatter_of_out_channel out in
  f fmt query;
  output_string out "\n";
  flush out;
  Unix.close where


let process out (query:Parse_arg.query) =
  let task = io.findlib query.task query.findlib in
  List.iter (Parse_arg.eval_single out query.params query.task) query.action.singles;
  if not (query.action.modes = [] && query.action.makefiles = [] ) then
    let analyzed = Analysis.main io query.params.analyzer task in
    List.iter (Parse_arg.iter_mode out query.params analyzed) query.action.modes;
    List.iter (Parse_arg.iter_makefile out query.params analyzed)
      query.action.makefiles


let rec server () =
  Unix.listen socket 10;
  match Unix.select [socket] [] [] 5. with
  | [_], _ , _  ->
    let client, _addr = Unix.accept socket in
    let _t = Thread.create (answer process) client in
    server ()
  | _ ->
    Unix.close socket;
    Unix.unlink uaddr


let () =
  Unix.bind socket addr;
  Unix.listen socket 10;
  server ()
