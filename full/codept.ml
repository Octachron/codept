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
  List.iter (Parse_arg.eval_single query.params query.task) query.action.singles;
  if not (query.action.modes = [] && query.action.makefiles = [] ) then
    let analyzed = Analysis.main query.params.analyzer task in
    List.iter (Parse_arg.iter_mode query.params analyzed) query.action.modes;
    List.iter (Parse_arg.iter_makefile query.params analyzed)
      query.action.makefiles
