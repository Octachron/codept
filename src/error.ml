  exception End_of_analysis
  let p fmt = Format.eprintf fmt
  let fp fmt = Format.fprintf fmt

  let log fmt = Format.kfprintf
      (fun _ppf -> raise End_of_analysis) Format.err_formatter
      ("@[<hov2>\\e[35mError\\e[39m:@,@ @["^^fmt^^"@]@]@.")

  let log_s s = log "%s" s
  let signature_expected () = log "Expected signature, got a functor"
  let module_type_error = log "Module type error: unknown module name %s"
  let not_a_functor () = log "Only functors can be applied"
