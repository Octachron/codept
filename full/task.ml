
(** Collecting data on the task at hand *)

open Common
open Params
module Pth=Paths.S


let add_invi task name =
  task := { !task with
            invisibles =
              Pth.Set.add
                (Paths.S.parse_filename name)
                (!task).invisibles
          }

let parse_filename s =
  match String.split_on_char '@' s with
  | [_] -> s, None
  | [a;b] ->
    a, Some (Namespaced.of_path @@ String.split_on_char '.' b)
  | _ -> assert false

let add_file kind format path task name =
  let k = { Common.kind ; format } in
  let files = (!task).files in
  task := { !task with files = (k,name,path) :: files }

let add_impl = add_file Implementation
let add_intf = add_file Interface
let add_sig = add_file Signature Read.M2l None

let add_seed _param task seed = (* TODO: namespaced seed *)
  let seed =
    Namespaced.make
    @@ String.capitalize_ascii
    @@ Support.remove_extension seed in
  task := { !task with seeds = seed :: (!task).seeds }

let file_path prefix name =
  Some (Namespaced.make ~nms:prefix @@
        Paths.S.(module_name @@ parse_filename name))

let add_file policy synonyms task path name k =
  if Sys.file_exists name then
    match Common.classify policy synonyms name with
    | None -> ( Fault.handle policy Codept_policies.unknown_extension name; k ())
    | Some { kind = Implementation; format } ->
      add_impl format path task name
    | Some { kind = Interface; format } ->
      add_intf format path task name
    | Some { kind = Signature; _ } ->
      add_sig task name


let rec add_file_rec ~prefix ~cycle_guard param task name0 =
  let name = String.concat "/" (prefix @ [name0]) in
  let lax = let open Fault in
    Policy.set_err (Codept_policies.unknown_extension, Level.info)
   L.(!param.[policy]) in
  let path = file_path L.(if !param.[nested] then prefix else []) name0 in
  let k () = if Sys.is_directory name then
        add_dir ~prefix ~cycle_guard param task ~dir_name:name0 ~abs_name:name in
  add_file lax L.(!param.[synonyms]) task path name k

and add_dir ~prefix ~cycle_guard param task
    ~dir_name ~abs_name =
    if  cycle_guard && dir_name = "." then
       ()
    else
      let cycle_guard = dir_name = "." in
      let files = Sys.readdir abs_name in
      Array.iter
        (add_file_rec ~prefix:(dir_name::prefix) ~cycle_guard
           param task)
        files

let add_file param task name0  =
  let name, path = parse_filename name0 in
  match path with
  | Some _  ->
    add_file L.(!param.[policy]) L.(!param.[synonyms]) task path name ignore
  | None ->
    add_file_rec ~prefix:[] ~cycle_guard:false param task name0

let add_impl param task name =
  let name, path = parse_filename name in
  add_impl param path task name

let add_intf param task name =
  let name, path = parse_filename name in
  add_intf param path task name

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
  L.( param.[transparent_aliases] <- true );
  add_invisible_file param task file

let as_map param task file =
  L.( param.[transparent_aliases] <- true ) ;
  add_file param task file
