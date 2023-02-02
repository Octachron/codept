let debug fmt =
  let pf = match Sys.getenv "CODEPT_DEBUG" with
    | "1" | "true" -> Format.fprintf
    | _ -> Format.ifprintf
    | exception Not_found -> Format.ifprintf in
  pf Pp.err ("Debug:" ^^ fmt ^^"@.")
