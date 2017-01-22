module Cmd = Arg
module U = Unit
module Pkg = Paths.Pkg
module Pth = Paths.Simple

(** Utility functions and module *)
open Params
let tool_name = "codept"
let version = 0.3
let stderr= Format.err_formatter
let std = Format.std_formatter


let () =
  let query = Parse_arg.process version Sys.argv in
  let task = Parse_arg.translate_findlib_query query.task query.findlib in
  let act = Parse_arg.act in
  List.iter Parse_arg.act query.action.action;
  if not (query.action.active_modes = []) then
    let analyzed = Analysis.main query.params.analyzer task in
    List.iter (fun f -> act @@ f analyzed) query.action.active_modes
