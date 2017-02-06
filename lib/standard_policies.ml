open Fault
open Policy
open Standard_faults


let default =
  let open Level in
  { silent = notification; exit = error;
    map = Level { expl = ""; lvl = Some critical } }
  |> set_err (applied_unknown, warning )
  |> set (["first_class"], Some "First-class module faults", warning )
  |> set_err (opened_first_class, warning)
  |> set_err (included_first_class, warning)
  |> set (["extension"],  Some "Extension node faults", warning)
  |> set_err (extension_ignored, warning)
  |> set_err (extension_traversed, notification)
  |> set (["parsing"], Some "Parsing faults",  error)
  |> register_err syntaxerr
  |> set_err (discordant_approximation, warning)
  |> set_err (concordant_approximation, notification)
  |> set_err (m2l_syntaxerr, error)
  |> set (["typing"], Some "Typing faults", warning)
  |> set_err (applied_structure, warning)
  |> set_err (structure_expected,warning)
  |> set_err (applied_unknown, notification)
  |> set_err (unknown_approximated, notification)
  |> set_err (ambiguous, warning)


let strict =
  let open Level in
  { default with exit = Level.notification }
  |> set (["typing"], Some "Typing faults", error)
  |> set_err (applied_structure, error)
  |> set_err (structure_expected,error)


let parsing_approx =
  default |> set_err (syntaxerr, Level.warning)


let lax =
  { parsing_approx with exit = Level.critical }

let quiet = { lax with silent = Level.error }
