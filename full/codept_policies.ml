open Fault
open Standard_faults
open Format_tags

let policy = Standard_policies.default
let unknown_extension =
  { path = ["codept"; "io"; "unknown extension"];
    expl = "Codept fault: attempting to read a file with an unknwon extension";
    log = (fun lvl ->
        log lvl "Attempting to read %a, aborting due to an unknown extension."
          (tagged Em)
      )
  }

let nonexisting_file =
  { path = ["codept"; "io"; "nonexisting file"];
    expl = "Codept fault: attempting to read a file which does not exist";
    log = (fun lvl -> log lvl "Attempting to read non-existing file %a."
              (tagged Em)
          )
  }


let invalid_file_group =
  { path = ["codept"; "io"; "invalid_file_group"];
    expl = "Codept fault: invalid syntax for a file group";
    log = (fun lvl x1 x2 -> log lvl
              "File group syntax error.@ Attempting to read %a,@ \
               aborting due to invalid syntax:@ %a."
              (tagged Em) x1 (tagged Em) x2
          )
  }

let policy =
  let open Policy in
  policy
  |> set_err (unknown_extension, Level.warning)
  |> set_err (nonexisting_file, Level.warning)
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
