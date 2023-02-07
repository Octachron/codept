type t = string

let msgf fmt = Format.kasprintf (fun msg -> `Msg msg) fmt

let for_all f str =
  let rec go acc idx =
    if idx < String.length str
    then go (f str.[idx] && acc) (succ idx)
    else acc in
  go true 0

let is_valid = function
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '_' | '\'' -> true
  | '-' -> true
    (* XXX(dinosaure): an example exists: [First-class-modules].
       [ocamlopt] can compile it but it emits an warning. *)
  | _ -> false

let is_upper = function
  | 'A' .. 'Z' -> true
  | _ -> false

let is_lower = function
  | 'a' .. 'z' -> true
  | _ -> false

let of_string str =
  if String.length str < 1
  then Error (msgf "Invalid empty module name")
  else if (is_upper str.[0] || is_lower str.[0]) && for_all is_valid str
  then Ok str else Error (msgf "Invalid module name: %S" str)

let v str = match of_string str with
  | Ok v -> v
  | Error (`Msg err) -> failwith err

let pp ppf t = Format.pp_print_string ppf t
let reflect ppf t = Pp.fp ppf "(Modname.v %S)" t

let of_path fpath =
  let fpath = Support.remove_extension (Filename.basename fpath) in
  v (String.capitalize_ascii fpath)

let to_string v = v
let invalid_arg fmt = Format.kasprintf invalid_arg fmt

let modulize str =
  if String.length str < 1
  then invalid_arg "Impossible to modulize an empty string";
  if not (is_upper str.[0] || is_lower str.[0])
  then invalid_arg "Impossible to modulize %S" str;
  let res = Bytes.create (String.length str) in
  for i = 0 to String.length str - 1 do
    if is_valid str.[i]
    then Bytes.set res i str.[i]
    else Bytes.set res i '_'
  done ; String.capitalize_ascii (Bytes.unsafe_to_string res)

let compare = String.compare

module Map = Map.Make (String)
