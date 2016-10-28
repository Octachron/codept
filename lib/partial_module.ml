module M = Module
module S = M.Sig

type t =
  { origin: M.origin;
    args: M.t option list;
    result: M.signature }
let empty = { origin = Submodule; args = []; result= S.empty }
let simple defs = { empty with result = defs }

let pp ppf (x:t) =
  if x.args = [] then
    Pp.fp ppf "%a(%a)" M.pp_signature x.result M.pp_origin x.origin
  else Pp.fp ppf "%a@,→%a(%a)"
      M.pp_args x.args
      M.pp_signature x.result
      M.pp_origin x.origin

let no_arg x = { origin = Submodule; args = []; result = x }

let drop_arg (p:t) = match  p.args with
  | _ :: args -> { p with args }
  | [] ->
    match p.origin with
    | Extern | First_class | Rec -> p (* we guessed the arg wrong *)
    | Unit _ | Submodule | Arg | Alias _ ->
      Error.log "Only functor can be applied, got:%a" pp p
(* there is an error somewhere *)

let to_module ?origin name (p:t) =
  let origin = match origin with
    | Some o -> M.at_most p.origin o
    | None -> p.origin
  in
  {M.name;origin; args = p.args; signature = p.result }

let to_arg name (p:t) =
  {M.name;origin=Arg; args = p.args; signature = p.result }

let of_module {M.args;signature;origin; _} = {origin;result=signature;args}

let is_functor x = x.args <> []

let to_sign fdefs =
  if fdefs.args <> [] then
    ( Pp.fp Pp.err "%a@." pp fdefs;
      Error.signature_expected ()
    )
  else
    fdefs.result

let to_defs fdefs = Definition.sg_bind @@ to_sign fdefs
