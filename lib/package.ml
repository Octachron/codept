type package = {name:string; content: content }
and content = Subpackages of package Name.Map.t | Modules of Module.t Name.Map.t

module Path = struct

  type package = Local | Unknown | Sys of Npath.t

  type t = { package: package ; file: Npath.t }
  type path = t
  let filename p =
    begin match p.package with
      | Sys n -> String.concat "/" n ^ "/"
      | _ -> ""
    end
      ^
      String.concat "/" p.file

  let is_known = function
    | {package=Unknown; _ } -> false
    | _ -> true

  let rec last = function
    | [a] -> a
    | [] -> raise  @@  Invalid_argument "last expected a non-empty-file"
    | _ :: q -> last q

  let may_chop_extension a =
    try Filename.chop_extension a with
      Invalid_argument _ -> a

  let module_name {file; _ } =
      String.capitalize_ascii @@ may_chop_extension @@ last @@ file

  let rec chop_suffix l = match l with
    | [] -> []
    | [a] -> [Filename.chop_extension a]
    | a :: q -> a :: chop_suffix q

  let rec change_file_extension ext = function
    | [] -> []
    | [a] -> [may_chop_extension a ^ ext ]
    | a :: q -> a :: change_file_extension ext q

  let change_extension ext p =
    { p with file = change_file_extension ext p.file }

  let cmo = change_extension ".cmo"
  let cmi = change_extension ".cmi"
  let cmx = change_extension ".cmx"


  let pp_package ppf = function
    | Local -> Pp.fp ppf "Local"
    | Unknown ->  Pp.fp ppf "Unknown"
    | Sys n -> Pp.fp ppf "Sys[%a]" Npath.pp n

  let pp_simple ppf {package;file}=
    Pp.fp ppf "(%a)%a" pp_package package Npath.pp file

  let pp ppf {package;file} =
    begin match package with
      | Local -> ()
      | Unknown -> Pp.fp ppf "?/"
      | Sys s ->
        Pp.fp ppf "%a/"
          Pp.(list ~sep:(s "/") string) s
    end;
    Pp.fp ppf "%a"
      Pp.(list ~sep:(s "/") string) file

  module Set = struct
    include Set.Make(struct type t = path let compare = compare end)
    let pp ppf s = Pp.(clist pp) ppf (elements s)
  end

  type set = Set.t

  let slash = '/'

  let parse_filename filename =
    let n = String.length filename in
    let rec extract shards start current =
      if current = n - 1  then
        List.rev @@ String.sub filename start (current-start-1) :: shards
      else if filename.[current] = slash then
        extract (String.sub filename start (current-start) :: shards)
          (current + 1) (current + 1)
      else
        extract shards start (current + 1) in
    extract [] 0 0

  let local file = { package = Local; file }
end
