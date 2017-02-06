
(** Collecting data on the task at hand *)

open Common
open Params
module Pth=Paths.S




let add_invi task name =
  task := { !task with
            invisibles = Pth.Set.add (Paths.S.parse_filename name) (!task).invisibles
          }

let add_file kind format task name =
  let k = { Common.kind ; format } in
  let files = (!task).files in
  task := { !task with files = (k,name) :: files }


let add_impl = add_file Implementation
let add_intf = add_file Interface
let add_sig = add_file Signature Read.M2l

let add_seed _param task seed =
  let seed = String.capitalize_ascii @@ Filename.remove_extension seed in
  task := { !task with seeds = seed :: (!task).seeds }

let rec add_file ~prefix ~cycle_guard ~policy param task name0 =
  let name = String.concat "/" (prefix @ [name0]) in
  let lax = let open Fault in
    Policy.set_err (Codept_policies.unknown_extension, Level.whisper)
      L.(!param.[policy]) in
  if Sys.file_exists name then
    match Common.classify lax L.(!param.[synonyms]) name with
    | None -> if Sys.is_directory name then
        add_dir ~prefix ~policy:lax ~cycle_guard param task
          ~dir_name:name0 ~abs_name:name
      else
        Fault.handle policy Codept_policies.unknown_extension name; ()
    | Some { kind = Implementation; format } ->
      add_impl format task name
    | Some { kind = Interface; format } -> add_intf format task name
    | Some { kind = Signature; _ } -> add_sig task name
and add_dir ~policy ~prefix ~cycle_guard param task ~dir_name ~abs_name =
    if  cycle_guard && dir_name = "." then
       ()
    else
      let cycle_guard = dir_name = "." in
      let files = Sys.readdir abs_name in
      Array.iter
        (add_file ~policy ~prefix:(dir_name::prefix) ~cycle_guard param task)
        files

let add_file param = add_file ~policy:L.(!param.[policy])
    ~cycle_guard:false ~prefix:[] param

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
