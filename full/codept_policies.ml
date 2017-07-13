open Fault
open Standard_faults
let policy = Standard_policies.default
let unknown_extension =
  { path = ["codept"; "io"; "unknown extension"];
    expl = "Codept fault: attempting to read a file with an unknwon extension";
    log = (fun lvl -> log lvl "Attempting to read %s, aborting due to \
                               an unknown extension.")
  }

let invalid_file_group =
  { path = ["codept"; "io"; "invalid_file_group"];
    expl = "Codept fault: invalid syntax for a file group";
    log = (fun lvl -> log lvl
              "File group syntax error.@ Attempting to read ⟨%s⟩, aborting due to invalid syntax:@ %s.")
  }

let policy =
  let open Policy in
  policy
  |> set_err (unknown_extension, Level.warning)
  |> set_err (invalid_file_group, Level.error)


let parsing_approx = let open Policy in
  policy |> set_err (syntaxerr, Level.warning)

let lax = { parsing_approx with exit = Level.critical }

let quiet = { lax with silent = Level.error }

let strict = let open Policy in
  { policy with exit = Level.notification }
  |> set (["typing"], Some "Typing faults", Level.error)
  |> set_err (applied_structure, Level.error)
  |> set_err (structure_expected, Level.error)
