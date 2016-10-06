let p fmt = Format.eprintf fmt
let fp fmt = Format.fprintf fmt


let log fmt = Format.eprintf @@ "@[<hov2>\x1b[35mWarning\x1b[39m:@,@ @["
                                ^^ fmt ^^ "@]@]@."

let log_s s = log "%s" s
let extension ()= log_s "extension node ignored."
let confused = log "confused author did not know what to do here: %s."
let first_class_module () =
  log_s "first-class modules are very partially handled for now."

let opened_first_class =
  log "First-class module %s was opened while its signature was unknown."

let included_first_class () =
  log "First-class module was included while its signature was unknown."
