
(** Collecting data on the task at hand *)

open Common
open Params
module Pth=Paths.S

let extension name =
  let n = String.length name in
  let r = try String.rindex name '.' with Not_found -> n-1 in
  String.sub name (r+1) (n-r-1)

let classify polycy synonyms f =
  let ext = extension f in
  match Name.Map.find ext synonyms with
  | x -> Some x
  | exception Not_found ->
    Fault.handle polycy Codept_polycy.unknown_extension ext; None


let add_invi task name =
  task := { !task with
            invisibles = Pth.Set.add (Paths.S.parse_filename name) (!task).invisibles
          }

let add_impl format task name =
  let k = { Read.kind = Structure; format } in
  let {Unit.ml;mli} = (!task).files in
  task := { !task with files = { ml = (k,name) :: ml; mli } }

let add_intf format task name =
  let k = { Read.kind = Signature; format } in
  let {Unit.ml;mli} = !(task).files in
  task := {!task with files = { mli = (k,name) :: mli; ml } }


let parse_sig lexbuf=
  Sexp.( (list Module.sexp).parse )
  @@ Sexp_parse.many Sexp_lex.main
  @@ lexbuf

let read_sigfile filename =
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
  let sigs = parse_sig lexbuf in
  close_in chan;
  sigs

let add_sig0 task more =
  let sigs = !(task).signatures in
  Option.iter (fun more ->
      task := {!task with signatures = more @ sigs  })
    more

let add_sig task ssig =
  add_sig0 task
  @@ parse_sig
  @@ Lexing.from_string ssig

let rec add_file ~prefix ~cycle_guard ~polycy param task name0 =
  let name = String.concat "/" (prefix @ [name0]) in
  if Sys.file_exists name then
    match classify polycy L.(!param.[synonyms]) name with
    | None -> if Sys.is_directory name then
        let polycy = let open Fault in
          Polycy.set_err (Codept_polycy.unknown_extension, Level.whisper)
            L.(!param.[polycy]) in
        add_dir ~prefix ~polycy ~cycle_guard param task
          ~dir_name:name0 ~abs_name:name
    | Some { kind = Implementation; format } ->
      add_impl format task name
    | Some { kind = Interface; format } -> add_intf format task name
    | Some { kind = Signature; _ } -> add_sig0 task @@ read_sigfile name
and add_dir ~polycy ~prefix ~cycle_guard param task ~dir_name ~abs_name =
    if  cycle_guard && dir_name = "." then
       ()
    else
      let cycle_guard = dir_name = "." in
      let files = Sys.readdir abs_name in
      Array.iter
        (add_file ~polycy ~prefix:(dir_name::prefix) ~cycle_guard param task)
        files

let add_file param = add_file ~polycy:L.(!param.[polycy])
    ~cycle_guard:false ~prefix:[] param

let add_invisible_file param task name =
  if Sys.file_exists name then
    ( add_invi task name;
      add_file  param task name
    )

let add_open task name =
  task := { !task with opens = [name] :: (!task).opens }

let lib task f =
  task := { !task with libs = f :: (!task).libs }

let map param task file =
  L.( param.[transparent_aliases] <- true );
  add_invisible_file param task file

let as_map param task file =
  L.( param.[transparent_aliases] <- true ) ;
  add_file param task file
