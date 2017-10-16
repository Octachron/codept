(** Compatibility function for manipulating out_format_functions *)

(** Replace the out_of_string field and
    setting out_spaces and out_ident —if it exists— to ignore *)
val transform:
  Format.formatter_out_functions
  -> (string -> int -> int -> unit )
  -> Format.formatter_out_functions
