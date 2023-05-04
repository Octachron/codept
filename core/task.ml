
(** Collecting data on the task at hand *)

open Common
open Params
module Pth=Paths.S

let add_invi task name =
  task := { !task with
            invisibles =
              Namespaced.Set.add
                (Namespaced.filepath_of_filename name)
                (!task).invisibles
          }

let find string c pos =
  try Some(String.index_from string pos c) with
  Not_found -> None
let first string cs pos =
  Array.fold_left (fun old x ->
      let new' = find string x pos in
      match old, new'  with
      | Some (_,o) , Some n ->
        if n < o then Some (x, n) else old
      | Some _ as s, None -> s
      | None, Some n -> Some(x,n)
      | None, None -> None
    ) None cs

let sub s start stop = String.sub s start (stop-start+1)

exception Invalid_file_group of string

type unit_desc =
  { filename: string; prefix:Paths.S.t; explicit_path: Namespaced.t option }

let parse_name name =
  match Support.split_on_char ':' name with
  | [_] -> { filename=name; prefix=[]; explicit_path=None }
  | [a;b] ->
    { filename = a; prefix = []; explicit_path= Some (Namespaced.of_path @@ Support.split_on_char '.' b)}
  | _ ->
    raise (Invalid_file_group "Multiple module path associated to the same file")

let invalid_arg fmt = Format.kasprintf invalid_arg fmt

let name_of_path v =
  match List.rev (Paths.S.parse_filename v) with
  | [] -> invalid_arg "Invalid empty module path: %S" v
  | x :: _ -> Modname.modulize (Support.remove_extension x)

let (#.) s (start,stop) = sub s start stop
let (--) start stop = start, stop
let (--*) start stop = start -- (stop-1)

let decorate m udesc =
  let nms = List.filter ((<>) "") @@
    List.map String.capitalize_ascii @@ Support.split_on_char '.' m in
  { udesc with prefix =  nms @ udesc.prefix }

let rec parse_top s pos =
  let len = String.length s in
  if pos >= len then
    len, []
  else match first s [|','; '['|] pos with
  | Some (',', p) ->
    let n, l as next = parse_top s (p+1) in
    if p = pos then next else
    n, parse_name s#.(pos--*p) :: l
  | Some ('[',p) ->
    parse_inner_group s parse_top pos p
  | _ ->  len, [parse_name s#.(pos--*len)]
and parse_group s pos =
  let len = String.length s in
  if pos >= len then len, []
   else begin
   match first s [|'[';','; ']'|] pos with
   | Some(',', p) ->
    let n, group as next = parse_group s (p+1) in
    if p = pos then next else
    n, parse_name s#.(pos--*p) :: group
  | Some (']', p) ->
    p+1, if p>pos then [parse_name s#.(pos--*p)] else []
  | Some ('[', p) -> parse_inner_group s parse_group pos p
  | _ -> raise (Invalid_file_group "Missing matching ']'")
end
and parse_inner_group s k pos p =
  let len = String.length s in
  if 1 + pos >= len then len, []
  else if p = pos then
    let after, inner = parse_group s (p+1) in
    let stop, rest = k s after in
    stop, inner @ rest
  else
    let m = String.capitalize_ascii s#.(pos--*p) in
    let after, group = parse_group s (p+1) in
    let add_inner l x = decorate m x :: l in
    let stop, inner = k s after in
    stop, List.fold_left add_inner inner group


let parse_filename s = snd @@ parse_top s 0

let add_file kind format task udesc =
  let path = match udesc.prefix, udesc.explicit_path with
    | [], None -> None
    | p, None ->
      Some (Namespaced.make ~nms:p (Modname.to_string (name_of_path udesc.filename)))
    | p, Some x -> Some Namespaced.{ x with namespace = p @ x.namespace }
  in
  let k = { Common.kind ; format } in
  let files = (!task).files in
  task := { !task with files = (k,udesc.filename,path) :: files }

let add_impl = add_file Implementation
let add_intf = add_file Interface
let add_sig task name=
add_file Signature Read.M2l task {filename=name; prefix=[]; explicit_path=None }

let add_seed _param task seed = (* TODO: namespaced seed *)
  let seed =
    Namespaced.make
    @@ String.capitalize_ascii
    @@ Support.remove_extension seed in
  task := { !task with seeds = seed :: (!task).seeds }

let add_file ?(explicit=false) k policy synonyms task udesc =
  if Sys.file_exists udesc.filename then
    match Common.classify policy synonyms udesc.filename with
    | None when Sys.is_directory udesc.filename -> k udesc.filename
    | None ->
      begin
        Fault.raise policy Codept_policies.unknown_extension udesc.filename;
        k udesc.filename
      end
    | Some { kind = Implementation; format } ->
      add_impl format task udesc
    | Some { kind = Interface; format } ->
      add_intf format task udesc
    | Some { kind = Signature; _ } ->
      add_sig task udesc.filename
  else if explicit then
    Fault.raise policy Codept_policies.nonexisting_file udesc.filename


let rec add_file_rec ~prefix:(mpre0,mpre1,fpre) ~start ~cycle_guard param task
    (name0,path) =
  let name = String.concat "/" (List.rev_append fpre [name0]) in
  let lax = let open Fault in
    Policy.register ~lvl:Level.info
      Codept_policies.unknown_extension
      L.(param#!policy) in
  let k name = if Sys.is_directory name then
        add_dir ~prefix:(mpre0, mpre1, fpre) start
          ~cycle_guard param task ~dir_name:name0 ~abs_name:name in
  add_file ~explicit:start k lax L.(param#!synonyms) task
    {filename=name; prefix= mpre0 @ List.rev mpre1; explicit_path=path}

and add_dir ~prefix:(mpre0,mpre1,fpre) first ~cycle_guard param task
    ~dir_name ~abs_name =
  if  cycle_guard && dir_name = "." then
    ()
  else
      let dir_name =
        if dir_name.[String.length dir_name - 1] = L.(param#!slash).[0] then
          String.sub dir_name 0 (String.length dir_name - 1)
        else
          dir_name
      in
      let cycle_guard = dir_name = "." in
      let files = Sys.readdir abs_name in

      let mpre1 =
        if L.(param#!nested ) && not first then
          let mname =  name_of_path dir_name in
          Modname.to_string mname :: mpre1
        else mpre1 in
      let f n = add_file_rec ~prefix:(mpre0, mpre1, dir_name :: fpre) ~start:false
          ~cycle_guard param task (n,None) in
      Array.iter f files

let add_file_rec ~prefix = add_file_rec ~prefix ~start:true ~cycle_guard:false;;

let add_file param task name0  =
  try let expanded = parse_filename name0 in
  let add udesc =
    match udesc.explicit_path with
    | None ->
      add_file_rec ~prefix:(udesc.prefix, [] ,[]) param task (udesc.filename, udesc.explicit_path)
    | Some p ->
      let module_prefix, module_name =
        let prefix =
          if Sys.is_directory udesc.filename then Namespaced.flatten p
          else p.Namespaced.namespace
        in
        prefix,
        { p with Namespaced.namespace = [] } in
      add_file_rec ~prefix:(udesc.prefix @ module_prefix,[],[]) param task
        (udesc.filename, Some module_name) in
    List.iter add expanded
  with Invalid_file_group s ->
    Fault.raise L.(param#!policy) Codept_policies.invalid_file_group (name0,s)

let add_impl param task name =
  List.iter (add_impl param task) (parse_filename name)

let add_intf param task name =
  List.iter (add_intf param task) (parse_filename name)

let add_invisible_file param task name =
  if Sys.file_exists name then
    ( add_invi task name;
      add_file  param task name
    )

let add_open task name =
  task := { !task with opens = [name] :: (!task).opens }

let lib task f =
  task := { !task with libs = (expand_dir f) :: (!task).libs }

let map param task file =
  L.( param#<-(transparent_aliases, true) );
  add_invisible_file param task file

let as_map param task file =
  L.( param#<-(transparent_aliases,true) ) ;
  add_file param task file
