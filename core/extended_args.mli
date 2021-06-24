val parse:
  string array
  -> (Arg.key * Arg.spec * Arg.doc) list
  -> (string -> unit)
  -> string
  -> unit
