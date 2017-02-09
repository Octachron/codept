open Fault
open Standard_faults
let policy = Standard_policies.default
let unknown_extension =
  { path = ["codept"; "io"; "unknown extension"];
    expl = "Codept fault: attempting to read a file with an unknwon extension";
    log = (fun lvl -> log lvl "Attempting to read %s, aborting due to \
                               an unknown extension.")
  }



let solver_error =
  { path = ["solver"; "block" ];
    expl = "Solver fault: major errors during analysis.";
    log = (fun lvl -> log lvl
              "Solver failure@?@[@<2> @[<0>@;%a@]@]" Solver.Failure.pp_cycle
          )
  }


let policy =
  let open Policy in
  policy
  |> set_err (unknown_extension, Level.warning)
  |> set_err (solver_error, Level.error)


let parsing_approx = let open Policy in
  policy |> set_err (syntaxerr, Level.warning)

let lax = { parsing_approx with exit = Level.critical }

let quiet = { lax with silent = Level.error }

let strict = let open Policy in
  { policy with exit = Level.notification }
  |> set (["typing"], Some "Typing faults", Level.error)
  |> set_err (applied_structure, Level.error)
  |> set_err (structure_expected, Level.error)
