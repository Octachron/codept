(** invariant defined ⊂ visible *)

module M = Module
module S = Module.Sig

type t = { defined: Module.signature; visible: Module.signature }
type definition = t

let empty = { defined = S.empty; visible = S.empty }


let only_visible defs =
  let diff = Name.Map.merge (fun _k x y ->
      match x, y with
      | Some _, (None|Some _) -> None
      | None , Some y -> Some y
      | None, None -> None ) in
  let d = defs.defined and v = defs.visible in
  { Module.modules = diff d.modules v.modules;
    module_types = diff d.module_types v.module_types
  }

let pp ppf x = Pp.fp ppf "defined:@[[@,%a@,]@]"
    Module.pp_signature x.defined;
  let v = only_visible x in
  if S.card v > 0 then
    Pp.fp ppf "@, visible:@[@,[%a@,]@]"
      Module.pp_signature v
  else ()


let sg_bind sign = { defined = sign; visible = sign }
let sg_see sign = { empty with visible = sign }

let clear_visible defs = { defs with visible = defs.defined }


module Def = struct
  let md m =
    sg_bind @@ S.create m

  let mods ms =
    sg_bind @@ S.of_list ms

  let sg m =
    sg_bind @@ S.create_type m

  let sgs ms =
    sg_bind @@ S.of_list_type ms

  let gen lvl = match lvl with
    | M.Module -> md
    | M.Module_type -> sg

  let (+@) = S.merge
  let ( +| ) d1 d2 =
   { defined =  d1.defined +@ d2.defined;
      visible = d1.visible +@ d2.visible }
end

let bind md defs =
  { visible = S.add defs.visible md;
    defined = S.add defs.defined md;
  }

let see md defs =
  { defs with
    visible = S.add defs.visible md }

let bind_sg md defs =
  { visible = S.add_type defs.visible md;
    defined = S.add_type defs.defined md }

let bind_gen level = match level with
  | M.Module -> bind
  | M.Module_type -> bind_sg

let binds l =
  List.fold_left (fun defs (level,md) -> bind_gen level md defs) empty l
