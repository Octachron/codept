type t =
  { modname : Modname.t
  ; filepath : string }
(* TODO(dinosaure): we probably should validate [filename] too as regular
   file path. *)

let invalid_arg fmt = Format.kasprintf invalid_arg fmt

let modulize filepath =
  let filename = Filename.basename filepath in
  let name = Support.remove_extension filename in
  if String.length name < 1
  then invalid_arg "Impossible to modulize an empty string";
  if not (Support.is_upper name.[0] || Support.is_lower name.[0])
  then invalid_arg "Impossible to modulize %S (from %S)" name filename;
  let modname = String.capitalize_ascii name in
  let modname = Modname.v modname in
  { modname; filepath; }

let pp ppf { modname; filepath; } =
  Pp.fp ppf "%a(%s)" Modname.pp modname filepath
let pp_as_modname ppf { modname; _ } = Modname.pp ppf modname
let pp_as_filepath ppf { filepath; _ } = Format.pp_print_string ppf filepath

let reflect ppf t =
  Pp.fp ppf "(Unitname.modulize %S)" t.filepath

let modname { modname; _ } = modname
let filename { filepath; _ } = Filename.basename filepath
let filepath { filepath; _ } = filepath
let compare_as_modnames a b = Modname.compare a.modname b.modname

let change_file_extension f t =
  match Support.extension t.filepath with
  | "" -> t
  | ext ->
    let filepath = (Support.remove_extension t.filepath) ^ "." ^ (f ext) in
    { t with filepath }

module Map = Map.Make (struct
  type nonrec t = t

  let compare a b = Modname.compare a.modname b.modname
end)

module Set = Set.Make (struct
  type nonrec t = t

  let compare a b = Modname.compare a.modname b.modname
end)
