(** Utility functions and module *)
open Params
let tool_name = "codept"
let version = 0.3
let stderr= Format.err_formatter
let std = Format.std_formatter

let io = Analysis.direct_io
let out = Pp.std

let () =
  let query = Parse_arg.process version Sys.argv in
  let task = io.findlib query.task query.findlib in
  List.iter (Parse_arg.eval_single out query.params query.task) query.action.singles;
  if not (query.action.modes = [] && query.action.makefiles = [] ) then
    let analyzed = Analysis.main io query.params.analyzer task in
    List.iter (Parse_arg.iter_mode out query.params analyzed) query.action.modes;
    List.iter (Parse_arg.iter_makefile out  query.params analyzed)
      query.action.makefiles
