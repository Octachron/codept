exception End_of_analysis
exception Opening_a_functor of string
exception Including_a_functor

exception Functor_expected of string
exception Functor_not_expected

let p fmt = Format.eprintf fmt
  let fp fmt = Format.fprintf fmt

  let log fmt = Format.kfprintf
      (fun _ppf -> raise End_of_analysis) Format.err_formatter
      ("@[<hov2>\x1b[31mError\x1b[39m:@,@ @["^^fmt^^"@]@]@.")

  let log_s s = log "%s" s
  let signature_expected () = log "Expected signature, got a functor"
  let module_type_error = log "Module type error: unknown module name %s"
  let not_a_functor () = log "Only functors can be applied"
