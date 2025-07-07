(** Utility functions and module *)
open Params
let tool_name = "codept"
let version = 0.11
let stderr= Format.err_formatter
let std = Format.std_formatter

let io = Io.direct
let out = Pp.std
let main () =
  let query = Args.process version Sys.argv in
  let simple = not query.params.pretty_format in
  Format_tags.enable ~simple Pp.err;
  Format_tags.enable ~simple Pp.std;

  let task = io.reader.findlib query.task query.findlib in
  Compenv.readenv stderr Before_link;
  List.iter (Args.eval_single out io.writer query.params query.task)
    query.action.singles;
  if not (query.action.modes = [] && query.action.makefiles = [] ) then
    let analyzed = Analysis.main io.reader query.params.analyzer task in
    List.iter (Args.iter_mode out io.writer query.params analyzed)
      query.action.modes;
    List.iter (Args.iter_makefile out  query.params analyzed)
      query.action.makefiles

let () = try main () with Fault.Fatal _ -> ()
