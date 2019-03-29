(** Analysis on single files *)

type t =
  | Approx_file
  | One_pass
  | M2l_info
  | M2l

type single = string -> Io.writer -> Format.formatter -> Params.t
  -> Common.info * string * Namespaced.t option -> unit

module Pkg = Paths.Pkg
open Params
open M2l

(** approx *)
let approx filename = Approx_parser.(to_upper_bound @@ lower_bound filename)

(** Printing directly from source file *)
let to_m2l policy sig_only (k,f,_n) =
  match Common.classic k with
  | None -> None
  | Some k ->
    match Read.file k f with
    | _name, Ok x ->
      if sig_only then Some (k, M2l.Sig_only.filter x) else Some (k,x)
    | _, Error (Ocaml (Syntax msg)) ->
      Fault.raise policy Standard_faults.syntaxerr msg;
      Some(k, approx f)
    | _, Error (Ocaml (Lexer msg)) ->
      Fault.raise policy Standard_faults.lexerr (!Location.input_name,msg);
      Some(k, approx f)
    | _, Error (Serialized e) ->
      Standard_faults.schematic_errors policy (f,"m2l",e); None

let approx_file _ _ ppf _param (_,f,_) =
  let _name, lower, upper = Approx_parser.file f in
  Pp.fp ppf  "lower bound:%a@. upper bound:%a@."
    M2l.pp lower M2l.pp upper

let one_pass _ _ ppf param (_,filename,_ as x) =
  let param = param.analyzer in
  let module Param = (val Analysis.lift param) in
  let module Sg = Outliner.Make(Envt.Core)(Param) in
  let start = to_m2l param.policy param.sig_only x in
  match
    Option.( start
             >>| snd
             >>| Sg.m2l (Pkg.local filename) Envt.Core.empty
             >>| Outliner.With_deps.unpack
           )
  with
  | None -> ()
  | Some (deps, Ok (_state, d)) ->
    Pp.fp ppf "Computation finished:@ %a@ %a@." Deps.pp deps Module.Sig.pp d
  | Some (_ , Error h) ->
    Pp.fp ppf "Computation halted at:\n %a@." M2l.pp h

let m2l_info _ _ ppf param f =
  let param = param.analyzer in
  let start = to_m2l param.policy param.sig_only f in
  let open Option in
  start
  >>| snd
  >>| Normalize.all
  >>| snd
  >>| Pp.fp ppf  "%a@." M2l.pp
  >< ()

let m2l filename (writer:Io.writer) ppf param f =
  let fmt = param.internal_format in
  let param = param.analyzer in
  let start = to_m2l param.policy param.sig_only f in
  let open Option in
  start
  >>| begin fun (k,m) ->
    m
    |> Normalize.all
    |> snd
    |> writer.m2l fmt (k,filename) ppf
  end
      >< ()


let eval = function
  | Approx_file -> approx_file
  | One_pass -> one_pass
  | M2l_info -> m2l_info
  | M2l -> m2l
