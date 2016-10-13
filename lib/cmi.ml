(** Extract module level definitions from cmi files *)

let cmi_infos path =
  Cmi_format.read_cmi path

let rec mmap f l = match l with
  | [] -> []
  | a :: q -> f a @ mmap f q


open Types


let bind id mt = let open M2l in
  Bind {name=id; expr = Constraint(Abstract,mt)}

let bind_sig id mt = let open M2l in
  Bind_sig { name = id; expr = Option.( mt >< Abstract)}


let module_declaration module_type id md = bind id @@ module_type md.md_type
let module_type_declaration module_type id mtd =
  bind_sig id Option.( mtd.mtd_type >>| module_type)

module P = Path

let rec from_path  =
  let open Epath in
  function
    | P.Pident s -> A s.name
    | P.Pdot (p,s,_n) -> S(from_path p,s)
    | P.Papply (f,x) -> F {f = from_path f; x=from_path  x}

module Arg = Module.Arg

let rec signitem =
  function
  | Sig_value _ (*val … *)
  | Sig_type _ (* type … *)
  | Sig_typext _ (* type 字 += … *)
  | Sig_class _ (* class … *)
  | Sig_class_type _ (* class …*) -> []
  | Sig_module  (id, md, _) ->
    [module_declaration module_type id.name md]
  | Sig_modtype (id, mtd) ->
    [module_type_declaration module_type id.name mtd]
and signature x = mmap signitem x
and module_type  = function
  | Mty_ident p -> M2l.Ident (from_path p)
  | Mty_signature s -> M2l.Sig (signature s)
  | Mty_functor ({name;_}, mto, mt) ->
    let open M2l in
    let arg = Option.( mto >>|module_type >>| fun s ->
                       {Arg.name; signature = s} ) in
    Fun { arg ; body = module_type mt}
  | Mty_alias p -> M2l.Alias (Epath.concrete @@ from_path p)
and module_type_opt mt =  Option.( mt >>| module_type >< Abstract)

let cmi_m2l path =
  let cm2i = cmi_infos path in
  signature @@ cm2i.cmi_sign
