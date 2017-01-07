open Fault
let polycy = Fault.Polycy.default
let unknown_extension =
  { path = ["codept"; "io"; "unknown extension"];
    expl = "Codept fault: attempting to read a file with an unknwon extension";
    log = (fun lvl -> log lvl "Attempting to read %s, aborting due to \
                               an unknown extension.")
  }


let m2l_syntaxerr =
  { path = ["codept"; "parsing"; "m2l"];
    expl = "Parsing fault: syntax error when parsing a m2l serialized file.";
    log = (fun lvl -> log lvl
              "Parsing fault: syntax error when parsing the m2l serialized \
               file %s."
          )
  }

let solver_error =
  { path = ["solver"; "block" ];
    expl = "Solver fault: major errors during analysis.";
    log = (fun lvl -> log lvl
              "Solver failure@?@[@<2> @[<0>@;%a@]@]" Solver.Failure.pp_cycle
          )
  }

let module_conflict =
  { path = ["codept"; "input"; "module conflict" ];
    expl = "A module name appears in multiple locations, only the first one will\
            be used in the following analysis.";
    log = (fun lvl name paths -> log lvl
              "Module conflict,@; Module %s is provided simultaneously by
@[<hov> %a@]" name Pp.(list ~pre:(s "(") ~sep:(s", ") ~post:(s")") Paths.P.pp) paths
          )
  }

let polycy =
  let open Polycy in
  polycy
  |> set_err (unknown_extension, Level.warning)
  |> set_err (m2l_syntaxerr, Level.warning)
  |> set_err (solver_error, Level.error)
  |> set_err (module_conflict, Level.warning)


let parsing_approx = let open Polycy in
  polycy |> set_err (syntaxerr, Level.warning)

let lax = { parsing_approx with exit = Level.critical }

let quiet = { lax with silent = Level.error }

let strict = let open Polycy in
  { polycy with exit = Level.notification }
  |> set (["typing"], Some "Typing faults", Level.error)
  |> set_err (applied_structure, Level.error)
  |> set_err (structure_expected, Level.error)
