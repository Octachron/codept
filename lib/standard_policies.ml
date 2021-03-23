open Fault
open Policy
open Standard_faults

open Level
let warn x = register ~lvl:warning x
let err x = register ~lvl:error x
let notif x = register ~lvl:notification x

let default =
  Policy.make ~silent:notification ~exit: error
  |> warn applied_unknown
  |> set ~lvl:warning ~expl:"First-class module faults" ["first_class"]
  |> warn opened_first_class
  |> warn included_first_class
  |> set ~lvl:warning ~expl:"Extension node faults" ["extension"]
  |> warn extension_ignored
  |> register ~lvl:notification extension_traversed
  |> set ["input"] ~expl:"Input faults"  ~lvl:error
  |> warn module_conflict
  |> err local_module_conflict
  |> set ["parsing"] ~expl:"Parsing faults" ~lvl:error
  |> err syntaxerr
  |> err lexerr
  |> warn discordant_approximation
  |> notif concordant_approximation
  |> err future_version
  |> err wrong_file_kind
  |> err unknown_file_format
  |> err parsing_error
  |> set ["typing"] ~expl:"Typing faults" ~lvl:warning
  |> warn applied_structure
  |> warn included
  |> warn nonexisting_submodule
  |> notif applied_unknown
  |> notif unknown_approximated
  |> warn ambiguous
  |> err Solver.fault



let strict =
  Policy.set_exit Level.notification default
  |> set ["typing"]  ~expl:"Typing faults" ~lvl:error
  |> err applied_structure
  |> err included


let parsing_approx =
  default |> register ~lvl:Level.warning syntaxerr


let lax =
  Policy.set_exit Level.critical parsing_approx

let quiet =
  Policy.set_silent Level.error lax
