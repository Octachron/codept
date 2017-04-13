(** invariant defined ⊂ visible *)

module M = Module
module S = Module.Sig

module Local = struct

  type t = { defined : Module.signature; visible: Module.signature; }
  let empty = { defined = S.empty; visible = S.empty }

  let clear_visible summary =
    { summary with visible = summary.defined }

  
  let merge d1 d2 =
   { defined = S.merge d1.defined d2.defined;
      visible = S.merge d1.visible d2.visible }
  
end
type local = Local.t =
  { defined : Module.signature; visible: Module.signature; }

module Namespace = struct
  type t = Paths.S.t
  type tree =
    | Node of tree Name.map
    | Leaf of Module.definition
  type map = tree Name.map
  let empty = Name.Map.empty
                
  let rec map_tree f = function
    | Leaf m -> Leaf (f m)
    | Node m -> Node (map f m)

   and map f m =
     Name.Map.fold
       (fun key value -> Name.Map.add key @@ map_tree f value )
       m Name.Map.empty
end

type t = { local:local; namespaces:Namespace.map }
type summary = t

let empty = { local = Local.empty; namespaces = Namespace.empty }
let active s = s.local
let defined s = s.local.defined

let only_visible_defs (defs:Local.t) =
  let diff = Name.Map.merge (fun _k x y ->
      match x, y with
      | Some _, (None|Some _) -> None
      | None , Some y -> Some y
      | None, None -> None ) in
  let d = S.flatten defs.defined and v = S.flatten defs.visible in
  M.Exact { Module.modules = diff d.modules v.modules;
            module_types = diff d.module_types v.module_types
          }
let pp_defs ppf (x:Local.t) = Pp.fp ppf "@[[@,%a@,]@]"
    Module.pp_signature x.defined;
  let v = only_visible_defs x in
  if S.card v > 0 then
    Pp.fp ppf "@, visible:@[@,[%a@,]@]"
      Module.pp_signature v
  else ()

let rec pp_nms ppf (n,t) = match t with
  | Namespace.Leaf defs -> Pp.fp ppf "@[%s:@;%a@]" n Module.Def.pp defs
  | Node m -> Pp.fp ppf "@[<v 2>%s:@;%a@]"
                n (Pp.list pp_nms) (Name.Map.bindings m)

let pp ppf summary =
  pp_defs ppf summary.local;
  if Name.Map.cardinal summary.namespaces > 0 then
    Pp.fp ppf "@;@[<v 2>%a@]" (Pp.list pp_nms)
      (Name.Map.bindings summary.namespaces)

module Sexp = struct
  open Sexp
  let def =
    convr
      (pair Module.Sig.sexp Module.Sig.sexp)
      (fun (a,b) -> {visible=b;defined=a})
      (fun d -> d.defined, d.visible)

  let leaf = C {
      name = "Leaf";
      proj = (function Namespace.Leaf l -> Some l | _ -> None);
      inj = (fun x -> Leaf x);
      impl = Module.Def.sexp;
      default = Some Module.Def.empty
    }

  let of_list = List.fold_left (fun m (k,x) -> Name.Map.add k x m)
      Name.Map.empty
      
  let rec node () = C {
      name = "Node";
      proj = (function Namespace.Node m -> Some m | _ -> None);
      inj = (fun x -> Namespace.Node x);
      impl = fix bmap;
      default = Some Name.Map.empty;
    }
  and tree () = sum [node (); leaf]
  and bmap () =
    convr
      (list @@ pair string @@ fix' tree)
      of_list
      Name.Map.bindings

  let local = Record.key Many "local" Local.empty
  let namespaces = Record.key Many "namespaces" Name.Map.empty

  let summary =
    let fr r = let open Record in
      create [ local := r.local; namespaces := r.namespaces ] in
    let f x = { local = Record.get local x;
                namespaces = Record.get namespaces x
              } in
    let record =
      let open Record in
      record [ field local def; field namespaces @@ fix bmap ] in
    conv {f;fr} record
  
end
let sexp = Sexp.summary

let local local = { empty with local }

let def_bind sign = { Local.defined = sign; visible = sign }
let sg_bind sign = local @@ def_bind sign
    
let def_see sign = { Local.empty with visible = sign }
let sg_see sign = local @@ def_see sign

let map_local f summary = 
  { summary with local = f summary.local }

let only_visible s = only_visible_defs @@ active s 

let rec in_namespace namespace s =
  match namespace with
  | [] -> { empty with local = s }
  | a :: q ->
    { local = Local.empty;
      namespaces =
        map_in_namespace q @@ Name.Map.singleton a (Namespace.Leaf s)
    }
and
  map_in_namespace l m = match l with
  | [] -> m
  | a :: q -> map_in_namespace q @@
    Name.Map.add a (Namespace.Node m) Name.Map.empty
  
let clear_visible = map_local Local.clear_visible 

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
  
  let merge s1 s2 =
    let rec tree_merge x y = match x, y with
      | Namespace.Leaf _, _ | _, Namespace.Leaf _ ->
        assert false (* FIXME *)
      | Namespace.( Node m, Node m' ) ->
        Namespace.Node (map_merge m m')
    and map_merge m m' = Name.Map.merge key_merge m m'
    and key_merge _key left right =
      match left, right with
      | Some a, Some b -> Some (tree_merge a b)
      | Some a, None | _, Some a -> Some a
      | None, None -> None in
    { local = Local.merge s1.local s2.local; 
      namespaces = map_merge s1.namespaces s2.namespaces }

  let (+|) = merge
end

let bind ?(namespace=[]) md summary =
  Def.merge summary @@ in_namespace namespace
  @@ def_bind @@ S.create md

let see ?(namespace=[]) md summary =
  Def.merge summary @@ in_namespace namespace
  @@ def_see @@ S.create md

let bind_sg ?(namespace=[]) md summary =
  Def.merge summary @@ in_namespace namespace
  @@ def_bind @@ S.create_type md

let bind_gen ?(namespace=[]) level = match level with
  | M.Module -> bind ~namespace
  | M.Module_type -> bind_sg ~namespace

let binds l =
  List.fold_left (fun summary (level,namespace,md) ->
      bind_gen ~namespace level md summary) empty l

let of_partial ?(namespace=[]) fsummary =
  let bind x = in_namespace namespace @@ def_bind x in 
  Mresult.fmap bind bind
  @@ M.Partial.to_sign fsummary
