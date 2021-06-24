open Fault
open Standard_faults
open Format_tags

let policy = Standard_policies.default
let unknown_extension =
  info ["codept"; "io"; "unknown extension"]
  "Codept fault: attempting to read a file with an unknwon extension"
    (fun ppf -> Format.fprintf ppf
        "Attempting to read %a, aborting due to an unknown extension."
        (tagged Em)
    )

let nonexisting_file =
  info ["codept"; "io"; "nonexisting file"]
   "Codept fault: attempting to read a file which does not exist"
   (fun ppf -> Format.fprintf ppf "Attempting to read non-existing file %a."
       (tagged Em)
   )


let invalid_file_group =
  info ["codept"; "io"; "invalid_file_group"]
    "Codept fault: invalid syntax for a file group"
    (fun ppf (x1,x2) -> Format.fprintf ppf
        "File group syntax error.@ Attempting to read %a,@ \
         aborting due to invalid syntax:@ %a."
        (tagged Em) x1 (tagged Em) x2
    )

let register = Policy.register
let policy =
  policy
  |> register ~lvl:Level.warning unknown_extension
  |> register ~lvl:Level.warning nonexisting_file
  |> register ~lvl:Level.error invalid_file_group


let parsing_approx =
  policy |> register ~lvl:Level.warning syntaxerr

let lax = Policy.set_exit Level.critical parsing_approx
let quiet = Policy.set_silent Level.error lax

let strict =
  Policy.set_exit Level.notification policy
  |> Policy.set ["typing"]  ~expl:"Typing faults" ~lvl:Level.error
  |> register ~lvl:Level.error applied_structure
  |> register ~lvl:Level.error included
