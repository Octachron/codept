(** Analysis on single files *)

module Pkg = Paths.Pkg
open Params
open M2l

(** Printing directly from source file *)
let to_m2l polycy sig_only (k,f) =
    match Read.file k f with
    | _name, Ok x ->
      if sig_only then Some (M2l.Sig_only.filter x) else Some x
    | _, Error (Ocaml msg) -> Fault.handle polycy Fault.syntaxerr msg; None
    | _, Error M2l -> Fault.handle polycy Codept_polycy.m2l_syntaxerr f; None


let approx_file ppf _param (_,f) =
  let _name, lower, upper = Approx_parser.file f in
  Pp.fp ppf  "lower bound:%a@. upper bound:%a@."
    M2l.pp lower M2l.pp upper

let one_pass ppf param ( (_,filename) as f) =
  let param = param.analyzer in
  let module Param = (val Analysis.lift param) in
  let module Sg = Interpreter.Make(Envts.Base)(Param) in
  let start = to_m2l param.polycy param.sig_only f in
  match Option.( start >>| Sg.m2l (Pkg.local filename) Envts.Base.empty ) with
  | None -> ()
  | Some (Ok (_state,d)) -> Pp.fp ppf "Computation finished:\n %a@." Module.Sig.pp d
  | Some (Error h) -> Pp.fp ppf "Computation halted at:\n %a@." M2l.pp h

let m2l ppf param f =
  let param = param.analyzer in
  let start = to_m2l param.polycy param.sig_only f in
  let open Option in
  start
  >>| Normalize.all
  >>| snd
  >>| Pp.fp ppf  "%a@." M2l.pp
  >< ()

let m2l_sexp ppf param f =
  let param = param.analyzer in
  let start = to_m2l param.polycy param.sig_only f in
  let open Option in
  start
  >>| Normalize.all
  >>| snd
  >>| M2l.sexp.embed
  >>| Pp.fp ppf  "%a@." Sexp.pp
  >< ()
