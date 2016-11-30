
let log fmt = Format.kfprintf
    (fun _ppf -> exit 1) Format.err_formatter
    ("@[[\x1b[31mError\x1b[39m]: @[<hov>"^^fmt^^"@]@]@.")

let log_s s = log "%s" s

let signature_expected () = log "Expected signature, got a functor"

let syntaxerr x = Syntaxerr.report_error Pp.err x; exit 1
