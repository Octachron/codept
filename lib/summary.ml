(** invariant defined ⊂ visible *)

module M = Module
module S = Module.Sig

type view = Module.signature
type t = {
  defined: view; (** modules and module types defined in scope *)
  visible: view; (** in scope but not defined *)
}
type summary = t

module View = struct

  let empty = Module.Sig.empty
  let is_empty v = v = empty

  let make_top ?(namespace=[]) x = S.create (M.with_namespace namespace x)
  let make sign = sign

  let merge s1 s2 =
    Module.Sig.merge s1 s2

  let strong_alias = function
    | M.Alias ({weak=true; _} as a) -> M.Alias { a with weak = false}
    | x -> x

  let rec strenghen = function
    | M.Blank -> M.Blank
    | Divergence ({before; after; _ } as d) ->
      Divergence { d with
                   after = str_def after;
                   before = strenghen before
                 }
    | Exact def -> Exact (str_def def)
  and str_def def =
    {def with modules = Name.Map.map strong_alias def.modules }

  let e = {visible = empty; defined = empty }
  let see visible = { e with visible }
  let define defined = { e with defined }

end

let empty = View.e
let strenghen v =
  { defined = View.strenghen v.defined;
    visible = View.strenghen v.visible;
  }

let peek x = x
let defined s = s.defined
let extend s y =
  S.merge (S.merge s y.defined) y.visible

let only_visible s = s.visible

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
    pp_view x.defined;
  if not @@ View.is_empty x.visible then
    Pp.fp ppf "@,in scope:@[@,[%a@,]@]"
      pp_view x.visible

let sch = let open Schematic in
  custom ["Summary"; "t"]
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
