(** invariant defined ⊂ visible *)

module M = Module
module S = Module.Sig

type view = Module.signature
type t = {
  defined: view; (** modules and module types defined in scope *)
  visible: view; (** all modules in scope: defined < visible *)
}
type summary = t

module View = struct

  let empty = Module.Sig.empty
  let is_empty v = v = empty

  let make sign = sign

  let merge s1 s2 =
    Module.Sig.merge s1 s2

  let e = {visible = empty; defined = empty }
  let see visible = { e with visible }
  let define defined = { defined; visible=defined }

  let pp = Module.Sig.pp
  let sch = Module.Sig.sch
end

let empty = View.e

let defined s = s.defined
let extend s y =
  S.merge s y.visible

let only_visible s =
  S.diff s.visible s.defined

(* REMOVE ME *)
(*
  let diff = Name.Map.merge (fun _k x y ->
      match x, y with
      | Some _, (None|Some _) -> None
      | None , Some y -> Some y
      | None, None -> None ) in
  let d = S.flatten defs.defined and v = S.flatten defs.visible in
  M.Exact { Module.modules = diff d.modules v.modules;
            module_types = diff d.module_types v.module_types
          }
*)


let pp_view ppf view =
  Module.Sig.pp ppf view

let pp ppf x = Pp.fp ppf "@[[@,%a@,]@]"
    pp_view x.visible

let sch = let open Schematic in
  custom
    [Module.Sig.sch;Module.Sig.sch]
    Tuple.(fun r -> [r.defined; r.visible] )
    Tuple.(fun [defined;visible] -> {defined;visible})

let clear_visible v = { v with visible =  View.empty }

let define ?(level=M.Module)  l =
  let open View in
  define @@ make @@
  match level with
  | M.Module -> S.of_list l
  | M.Module_type -> S.of_list_type l

let merge s1 s2 = {
  visible = View.merge s1.visible s2.visible;
  defined = View.merge s1.defined s2.defined;
}

let (+|) = merge

let add create view md summary =
  merge summary @@ view @@
  create md

let bind_md = add S.create View.define
let see = add S.create View.see

let bind_sg = add S.create_type View.define


let bind ?(level=M.Module) = match level with
  | M.Module -> bind_md
  | M.Module_type -> bind_sg

let binds l =
  List.fold_left (fun summary (level,md) ->
      bind ~level md summary) empty l

let of_partial fsummary =
  let bind x = View.define @@ View.make x in
  Mresult.fmap bind bind
  @@ M.Partial.to_sign fsummary
