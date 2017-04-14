(** invariant defined ⊂ visible *)

module M = Module
module S = Module.Sig

module Namespace = struct
  type t = Paths.S.t
  type tree =
    | Node of tree Name.map
    | Leaf of Module.signature
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

type view = { local: Module.signature; namespace: Namespace.map }
type t = {
  defined: view; (** modules and module types defined in scope *)
  visible: view; (** in scope but not defined *)
}
type summary = t

module View = struct

  let empty = { local = Module.Sig.empty; namespace= Namespace.empty }
  let is_empty v =
    Module.Sig.empty = v.local && v.namespace = Namespace.empty

  let rec make_namespace sign = function
    | [] -> assert false
    | [a] -> Name.Map.singleton a (Namespace.Leaf sign)
    | a :: q ->
      Name.Map.singleton a @@ Namespace.Node(make_namespace sign q)

  let make ?(namespace=[]) local =
    match namespace with
    | [] -> { local; namespace = Namespace.empty }
    | _ :: _ as l ->
      { local = Module.Sig.empty;
        namespace = make_namespace local l
      }


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
    { local = Module.Sig.merge s1.local s2.local;
      namespace = map_merge s1.namespace s2.namespace }

  let see v = { visible = v; defined = empty }
  let define v = { visible = empty; defined = v }

end

let empty = { defined = View.empty; visible = View.empty }
let defined s = s.defined
let local v = v.local

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


let rec pp_nms ppf (n,t) = match t with
  | Namespace.Leaf defs ->
    Pp.fp ppf "@[%s:@;%a@]" n Module.Sig.pp defs
  | Node m -> Pp.fp ppf "@[<v 2>%s:@;%a@]"
                n (Pp.list pp_nms) (Name.Map.bindings m)

let pp_view ppf view =
  Module.Sig.pp ppf view.local;
  if Name.Map.cardinal view.namespace > 0 then
    Pp.fp ppf "@;@[<v 2>%a@]" (Pp.list pp_nms)
      (Name.Map.bindings view.namespace)

let pp ppf x = Pp.fp ppf "@[[@,%a@,]@]"
    pp_view x.defined;
  if not @@ View.is_empty x.visible then
    Pp.fp ppf "@, in scope:@[@,[%a@,]@]"
      pp_view x.visible
  else ()


module Sexp = struct
  open Sexp

  let leaf = C {
      name = "Leaf";
      proj = (function Namespace.Leaf l -> Some l | _ -> None);
      inj = (fun x -> Leaf x);
      impl = Module.Sig.sexp;
      default = Some Module.Sig.empty
    }

  let of_list = List.fold_left (fun m (k,x) -> Name.Map.add k x m)
      Namespace.empty

  let rec node () = C {
      name = "Node";
      proj = (function Namespace.Node m -> Some m | _ -> None);
      inj = (fun x -> Namespace.Node x);
      impl = fix bmap;
      default = Some Namespace.empty;
    }
  and tree () = sum [node (); leaf]
  and bmap () =
    convr
      (list @@ pair string @@ fix' tree)
      of_list
      Name.Map.bindings

  let local = Record.key Many "local" Module.Sig.empty
  let namespace = Record.key Many "namespaces" Namespace.empty

  let view =
    let fr r = let open Record in
      create [ local := r.local; namespace := r.namespace ] in
    let f x = { local = Record.get local x;
                namespace = Record.get namespace x
              } in
    let record =
      let open Record in
      record [ field local Module.Sig.sexp;
               field namespace @@ fix bmap ] in
    conv {f;fr} record

    let summary =
    convr
      (pair view view)
      (fun (a,b) -> {visible=b;defined=a})
      (fun d -> d.defined, d.visible)

end
let sexp = Sexp.summary

let clear_visible v = { v with visible =  View.empty }

let define ?(level=M.Module)  l =
  let open View in
  define @@ make @@
  match level with
  | M.Module -> S.of_list l
  | M.Module_type -> S.of_list_type l

let merge s1 s2 = {
  visible = View.merge s1.visible s2.visible;
  defined = View.merge s1.defined s2.defined
}

let (+|) = merge

let add create view ?(namespace=[]) md summary =
  merge summary @@ view @@
  View.make ~namespace @@ create md

let bind_md = add S.create View.define
let see = add S.create View.see

let bind_sg = add S.create_type View.define


let bind ?(namespace=[]) ?(level=M.Module) = match level with
  | M.Module -> bind_md ~namespace
  | M.Module_type -> bind_sg ~namespace

let binds l =
  List.fold_left (fun summary (level,namespace,md) ->
      bind ~namespace ~level md summary) empty l

let of_partial ?(namespace=[]) fsummary =
  let bind x = View.define @@ View.make ~namespace x in
  Mresult.fmap bind bind
  @@ M.Partial.to_sign fsummary
