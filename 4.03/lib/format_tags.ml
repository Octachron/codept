type t =  ..
type t +=
  | Info | Notification | Warning | Error | Critical
  | Em | Loc | Title | M

let of_string =
  function
  | "info" | "0" -> Info
  | "notification" | "1" -> Notification
  | "warning" | "2" -> Warning
  | "error" | "3" -> Error
  | "critical" | "4" -> Critical
  | "em" -> Em
  | "loc" -> Loc
  | "title" -> Title
  | "m" -> M
  | _ -> Info

let to_string = function
  | Info -> "info"
  | Notification -> "notification"
  | Warning -> "warning"
  | Error -> "error"
  | Critical -> "critical"
  | Em -> "em"
  | Loc -> "loc"
  | Title -> "title"
  | M -> "m"
  | _ -> "?"


let mark_open_tag tag =
  let b = "\x1b[1m" in
  match of_string tag with
  | Critical -> b ^ "\x1b[91m"
  | Error -> b ^ "\x1b[31m"
  | Warning -> b ^ "\x1b[35m"
  | Notification -> b ^ "\x1b[36m"
  | Info -> b
  | Em -> b (* "\x1b[3m" *)
  | Loc -> b
  | Title -> b
  | M -> b
  | _ -> b

let mark_close_tag _tag =
  "\x1b[0m"

let enable ppf =
  Format.pp_set_tags ppf true;
  Format.pp_set_mark_tags ppf true;
  Format.pp_set_formatter_tag_functions ppf
    { (Format.pp_get_formatter_tag_functions ppf ()) with
      mark_open_tag; mark_close_tag }

let with_tag tag printer ppf x =
  Format.pp_open_tag ppf (to_string tag);
  printer ppf x;
  Format.pp_close_tag ppf ()

let tagged tag ppf x =
  Format.pp_open_tag ppf (to_string tag);
  Format.pp_print_string ppf x;
  Format.pp_close_tag ppf ()
