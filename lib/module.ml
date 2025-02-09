module Arg = struct
  type 'a t = { name:Name.t option; signature:'a }
  type 'a arg = 'a t

  let pp pp ppf = function
    | Some arg ->
      Pp.fp ppf "(%a:%a)" Name.pp_opt arg.name pp arg.signature
    | None -> Pp.fp ppf "()"

  let sch sign = let open Schematic.Tuple in
    let fwd arg = [arg.name; arg.signature] in
    let rev [name;signature] = {name;signature} in
    Schematic.custom  Schematic.[option String; sign] fwd rev

  let map f x = { x with signature = f x.signature }

  let reflect pp ppf = function
    | Some arg ->
      Pp.fp ppf {|Some {name="%a"; %a}|} Name.pp_opt arg.name pp arg.signature
    | None -> Pp.fp ppf "()"

  let pp_s pp_sig ppf args = Pp.fp ppf "%a"
      (Pp.(list ~sep:(s "→@,")) @@ pp pp_sig) args;
    if List.length args > 0 then Pp.fp ppf "→"
end


module Divergence= struct
  type origin =
    | First_class_module
    | External
  type t =  { root: Name.t option; origin:origin; loc: Uloc.t }

  let pp_origin ppf s =
    Pp.fp ppf "%s" @@
    match s with
    | First_class_module -> "first class module"
    | External -> "external module"


  module Reflect = struct

  let origin_r ppf s =
    Pp.fp ppf "%s" @@
    match s with
    | First_class_module -> "First_class_module"
    | External -> "External"

  let rloc ppf =
    let open Loc in
    function
    | Nowhere -> Pp.fp ppf "Nowhere"
    | Simple {line;start;stop} -> Pp.fp ppf "Simple{line=%d;start=%d;stop=%d}"
                                    line start stop
    | Multiline {start; stop} ->
      let pair ppf (x,y)= Pp.fp ppf "(%d,%d)" x y in
      Pp.fp ppf "Multiline{start=%a; stop =%a}"
        pair start pair stop

  let floc ppf {Uloc.pkg; loc} =
    Pp.fp ppf "(%a:%a)" Pkg.reflect pkg rloc loc

  let divergence ppf {root;loc;origin} =
    Pp.fp ppf "{root=%a;loc=%a;origin=%a}" Pp.estring Option.(root><"")
      floc loc origin_r origin

  end
  let reflect = Reflect.divergence

  let pp ppf {root; origin; loc= {pkg=path;loc} } =
    Pp.fp ppf "open %s at %a:%a (%a)"
      Option.(root><"")
      Pkg.pp path Loc.pp loc
      pp_origin origin

  let sch_origin =
    let open Schematic in
    custom
      (Sum[ "First_class_module",  Void; "External", Void])
      (function
        | First_class_module -> C E
        | External -> C (S E))
      (function
      | C E -> First_class_module
      | C (S E) -> External
      | _ -> .)

  let sch = let open Schematic in let open Tuple in
    custom
      Schematic.[option String; sch_origin; [Pkg.sch; Loc.Sch.t ]]
      (fun r -> [r.root;r.origin; [r.loc.pkg; r.loc.loc] ])
      (fun [root;origin;[pkg;loc]] -> {root;origin;loc={pkg;loc}} )

end


module Origin = struct

  type t =
    | Unit of {source:Pkg.t; path:Namespaced.t}
    (** aka toplevel module *)
    | Submodule
    | Namespace (** Temporary module from namespace *)
    | First_class (** Not resolved first-class module *)
    | Arg (** functor argument *)
    | Phantom of bool * Divergence.t
    (** Ambiguous module, that could be an external module *)

  let pp ppf = function
    | Unit {source; path} ->
      begin match source.Pkg.source with
        | Pkg.Local-> Pp.fp ppf "#%a" Namespaced.pp path
        | Pkg x ->
          Pp.fp ppf "#[%a]%a"
            Namespaced.pp x Namespaced.pp path
        | Unknown -> Pp.fp ppf "#!%a" Namespaced.pp path
        | Special n -> Pp.fp ppf "*(%s)%a" n Namespaced.pp path
      end
    | Submodule -> Pp.fp ppf "."
    | Namespace -> Pp.fp ppf "(nms)"
    | First_class -> Pp.fp ppf "'"
    | Arg -> Pp.fp ppf "§"
    | Phantom _ -> Pp.fp ppf "👻"

  module Sch = struct open Schematic
    let raw =
      Sum [ "Unit", [Pkg.sch; Namespaced.sch];
            "Submodule", Void; "First_class", Void; "Arg", Void;
            "Phantom", [ Bool; Divergence.sch];
            "Namespace", Void
          ]
    let t = let open Tuple in
      custom raw
        (function
          | Unit {source; path} -> C (Z [source;path])
          | Submodule -> C (S E)
          | First_class -> C (S (S E))
          | Arg -> C(S (S (S E)))
          | Phantom (b,div) -> C (S (S (S (S(Z [b;div])))))
          | Namespace -> C (S (S (S (S (S E)))))
        )
        (function
          | C Z [source;path] -> Unit {source;path}
          | C S E -> Submodule
          | C S S E -> First_class
          | C S S S E -> Arg
          | C S S S S Z [b;d] -> Phantom(b,d)
          | C S S S S S E -> Namespace
          | _ -> .
        )
  end let sch = Sch.t

    let reflect ppf = function
      | Unit u  -> Pp.fp ppf "Unit {source=%a;path=%a}"
                     Pkg.reflect u.source Namespaced.reflect u.path
    | Submodule -> Pp.fp ppf "Submodule"
    | First_class -> Pp.fp ppf "First_class"
    | Arg -> Pp.fp ppf "Arg"
    | Phantom (root,b) -> Pp.fp ppf "Phantom (%b,%a)" root Divergence.reflect b
    | Namespace -> Pp.fp ppf "Namespace"


  let at_most max v = match max, v with
    | (First_class|Arg|Namespace) , _ -> max
    | Unit _ , v -> v
    | Submodule, Unit _ -> Submodule
    | Phantom _, Submodule -> Submodule
    | Submodule, v -> v
    | Phantom _ as ph , _ -> ph
end
type origin = Origin.t


(** Type-level tags *)

type extended = private Extended
type simple = private Simple

(** Signature with tracked origin *)
type tracked_signature = {
  origin : Origin.t;
  signature : signature;
}


(** Core module or alias *)
and _ ty =
  | Sig: tracked_signature -> 'any ty (** Classic module *)
  | Alias:
      {
        path: Namespaced.t;
        (** Path.To.Target:
            projecting this path may create new dependencies
        *)
        phantom: Divergence.t option;
        (** Track potential delayed dependencies
            after divergent accident:
            [
              module M = A  (* Alias { name = M; path = [A] } *)
              open Unknownable (* <- divergence *)
              open M (* Alias{ phantom = Some divergence } *)
            ]
            In the example above, [M] could be the local module
            [.M], triggering the delayed alias dependency [A]. Or it could
            be a submodule [Unknownable.M] . Without sufficient information,
            codept defaults to computing an upper bound of dependencies,
            and therefore considers that [M] is [.M], and the inferred
            dependencies for the above code snipet is {A,Unknowable} .
        *)
      } -> extended ty

  | Abstract: Id.t -> 'any ty
  (** Abstract module type may be refined during functor application,
      keeping track of their identity is thus important
  *)

  | Fun: 'a ty Arg.t option * 'a ty -> 'a ty

  | Link: Namespaced.t -> extended ty (** Link to a compilation unit *)
  | Namespace: dict -> extended ty
  (** Namespace are open bundle of modules *)

and t = extended ty

and definition = { modules : dict; module_types : dict }
and signature =
  | Blank (** Unknown signature, used as for extern module, placeholder, … *)
  | Exact of definition
  | Divergence of { point: Divergence.t; before:signature; after:definition}
  (** A divergent signature happens when a signature inference is disturbed
      by opening or including an unknowable module:
      [ module A = …
         include Extern (* <- divergence *)
        module B = A (* <- which A is this: .A or Extern.A ?*)
      ]
  *)

and dict = t Name.map

type sty = simple ty
type level = Module | Module_type
type modul_ = t
type named = Name.t * t


let is_functor = function
  | Fun _ -> true
  | _ -> false

module Dict = struct
  type t = dict
  let empty = Name.Map.empty
  let of_list = List.fold_left (fun x (name,m) -> Name.Map.add name m x) empty

  let union =
    let rec merge _name x y = match x, y with
      | (Sig { origin = Unit {path = p;_}; _ } as x), Link path
        when path = p -> Some x
      (*      | x, Alias {weak=true; _ } -> Some x *)
      | Namespace n, Namespace n' ->
        Some (Namespace (Name.Map.union merge n n'))
      | _, r -> Some r in
    Name.Map.union merge

  let weak_union =
    let rec merge _k x y = match x, y with
      | Namespace n, Namespace n' ->
        Some (Namespace (Name.Map.union merge n n'))
      | x, _ -> Some x in
    Name.Map.union merge

  let diff x y = Name.Map.merge ( fun _ x y -> match x, y with
      | Some _, Some _ -> None
      | Some _ as x, None -> x
      | None, _ -> None
    ) x y

  let sch elt =
    let open Schematic in
    Custom {
      fwd = Name.Map.bindings;
      rev = of_list;
      sch = Array (pair String elt)}
end

(* TODO: Behavior with links *)
let rec spirit_away breakpoint root = function
  | Alias a as al ->
    if not root then
      Alias { a with phantom = Some breakpoint }
    else al
  | Abstract _ | Fun _ as f -> f
  | Link _ as l -> l
  | Namespace modules ->
    Namespace ( Name.Map.map (spirit_away breakpoint false) modules )
  | Sig m ->
    let origin = Origin.Phantom (root,breakpoint) in
    let origin = match m.origin with
      | Unit _  as u -> u
      | Phantom _ as ph -> ph
      | _ -> origin in
    Sig { origin; signature = spirit_away_sign breakpoint false m.signature }
and spirit_away_sign breakpoint root = function
  | Blank -> Blank
  | Divergence d -> Divergence {
      before = spirit_away_sign breakpoint root d.before;
      point = d.point;
      after = spirit_away_def breakpoint root d.after
    }
  | Exact def -> Exact (spirit_away_def breakpoint root def)
and spirit_away_def breakpoint root def =
  let map root =
    Name.Map.map (spirit_away breakpoint root) in
  { modules = map root def.modules; module_types = map true def.module_types }

let spirit_away b =  spirit_away b true

let sig_merge (s1: definition) (s2:definition) =
  { module_types = Name.Map.union' s1.module_types s2.module_types;
    modules = Dict.union s1.modules s2.modules }

let sig_diff s1 s2 =
  {
    module_types = Dict.diff s1.module_types s2.module_types;
    modules = Dict.diff s1.modules s2.modules
  }

let empty = Name.Map.empty
let empty_sig = {modules = empty; module_types = empty }

let rec flatten = function
  | Exact x -> x
  | Divergence d -> sig_merge (flatten d.before) d.after
  | Blank -> empty_sig

let is_exact_sig = function
  | Exact _ -> true
  | Divergence _ -> false
  | Blank -> false

let is_exact m =
  match m with
  | Namespace _ | Link _ | Abstract _  | Fun _ -> true
  | Alias {phantom ; _ } -> phantom = None
  | Sig m -> is_exact_sig m.signature

let md s = Sig s

let rec aliases0 l = function
  | Alias {path; _ } | Link path -> path :: l
  | Abstract _ | Fun _ -> l
  | Namespace modules ->
      Name.Map.fold (fun _ x l -> aliases0 l x) modules l
  | Sig { signature; _ } ->
    let signature = flatten signature in
    let add _k x l = aliases0 l x in
    Name.Map.fold add signature.modules
    @@ Name.Map.fold add signature.module_types
    @@ []

let aliases = aliases0 []


let pp_alias = Pp.opt Paths.Expr.pp

let pp_level ppf lvl =  Pp.fp ppf "%s" (match lvl with
    | Module -> "module"
    | Module_type -> "module type"
  )

let reflect_phantom ppf = function
  | None -> Pp.fp ppf "None"
  | Some x -> Pp.fp ppf "Some(%a)" Divergence.reflect x

let reflect_opt reflect ppf = function
  | None -> Pp.string ppf "None"
  | Some x -> Pp.fp ppf "Some %a" reflect x

let rec reflect ppf = function
  | Sig m ->  Pp.fp ppf "Sig (%a)" reflect_m m
  | Fun (arg,x) ->  Pp.fp ppf "Fun (%a,%a)" (reflect_opt reflect_arg) arg reflect x
  | Namespace modules ->
    Pp.fp ppf "Namespace (%a)"
      reflect_mdict modules
  |  Alias {path;phantom} ->
    Pp.fp ppf "Alias {path=%a;phantom=%a}"
      reflect_namespaced path
      reflect_phantom phantom
  | Link path ->
    Pp.fp ppf "Link (%a)"
      reflect_namespaced path
  | Abstract n -> Pp.fp ppf "Abstract %a" Id.pp n
and reflect_named ppf (n,m) = Pp.fp ppf "(%S,%a)" n reflect m
and reflect_namespaced ppf nd =
  if nd.namespace = [] then
    Pp.fp ppf {|Namespaced.make "%a"|}
      Unitname.pp_as_modname nd.name
  else
    Pp.fp ppf {|Namespaced.make ~nms:[%a] "%a"|}
      Pp.(list ~sep:(s";@ ") @@ estring) nd.namespace
      Unitname.pp_as_modname nd.name
and reflect_m ppf {origin;signature} =
  Pp.fp ppf {|@[<hov>{origin=%a; signature=%a}@]|}
    Origin.reflect origin
    reflect_signature signature
and reflect_signature ppf m = reflect_definition ppf (flatten m)
and reflect_definition ppf {modules; module_types} =
  match Name.Map.cardinal modules, Name.Map.cardinal module_types with
  | 0, 0 -> Pp.string ppf "empty"
  | _, 0 -> Pp.fp ppf
              "of_list @[<hov>[%a]@]" reflect_mdict modules
  | 0, _ -> Pp.fp ppf
              "of_list_type @[<hov>[%a]@]"
              reflect_mdict module_types
  | _ ->
    Pp.fp ppf "@[(merge @,(of_list [%a]) @,(of_list_type [%a])@, )@]"
      reflect_mdict modules
      reflect_mdict module_types
and reflect_mdict ppf dict =
      Pp.(list ~sep:(s ";@ ") @@ reflect_named) ppf (Name.Map.bindings dict)
and reflect_arg ppf arg = Pp.fp ppf "{name=%a;signature=%a}"
    (reflect_opt Pp.estring) arg.name reflect arg.signature

let reflect_modules ppf dict =
  Pp.fp ppf "Dict.of_list @[<v 2>[%a]@]"
    (Pp.list ~sep:(Pp.s ";@ ") reflect_named)
    (Name.Map.bindings dict)

let rec pp ppf = function
  | Alias {path;phantom} ->
    Pp.fp ppf "≡%s%a" (if phantom=None then "" else "(👻)" )
      Namespaced.pp path
  | Link path -> Pp.fp ppf "⇒%a" Namespaced.pp path
  | Sig m -> pp_m ppf m
  | Fun (arg,x) ->
    Pp.fp ppf "%a->%a" Pp.(opt pp_arg) arg pp x
  | Namespace n -> Pp.fp ppf "Namespace @[[%a]@]"
                     pp_mdict n
  | Abstract n -> Pp.fp ppf "■(%a)" Id.pp n


and pp_m ppf {origin;signature;_} =
  Pp.fp ppf "%a:%a"
    Origin.pp origin pp_signature signature
and pp_signature ppf = function
  | Blank -> Pp.fp ppf "ø"
  | Exact s -> pp_definition ppf s
  | Divergence {point; before; after} ->
      Pp.fp ppf "%a ∘ %a ∘ %a"
      pp_signature before
      Divergence.pp point
      pp_definition after
and pp_definition ppf {modules; module_types} =
  Pp.fp ppf "@[<hv>(%a" pp_mdict modules;
  if Name.Map.cardinal module_types >0 then
    Pp.fp ppf "@, types:@, %a)@]"
      pp_mdict module_types
  else Pp.fp ppf ")@]"
and pp_mdict ppf dict =
  Pp.fp ppf "%a" (Pp.(list ~sep:(s " @,")) pp_pair) (Name.Map.bindings dict)
and pp_pair ppf (name,md) = Pp.fp ppf "%s:%a" name pp md
and pp_arg ppf arg = Pp.fp ppf "(%a:%a)" (Pp.opt Pp.string) arg.name pp arg.signature



let mockup ?origin ?path name =
  let origin = match origin, path with
    | _, Some p -> Origin.Unit {source= p; path=Namespaced.make name}
    | Some o, None -> o
    | _ -> Submodule in
  {
    origin;
    signature = Blank
  }

let create
    ?(origin=Origin.Submodule) signature =
  { origin; signature}

let namespace (path:Namespaced.t) =
  let rec namespace (global:Namespaced.t) path =
    match path with
    | [] -> raise (Invalid_argument "Module.namespace: empty namespace")
    | [name] ->
      name, Namespace (Dict.of_list [Modname.to_string (Unitname.modname global.name), Link global])
    | name :: rest ->
      name, Namespace (Dict.of_list [namespace global rest])
  in
  namespace path path.namespace

let rec with_namespace nms name module'=
  match nms with
  | [] -> name, module'
  | a :: q ->
    let sub = with_namespace q name module' in
    a, Namespace (Dict.of_list [sub])


let signature_of_lists ms mts =
  let e = Name.Map.empty in
  let add map (name,m) = Name.Map.add name m map in
  { modules = List.fold_left add e ms;
    module_types = List.fold_left add e mts
  }


let to_list m = Name.Map.bindings m

module Schema = struct
  open Schematic
  module Origin_f = Label(struct let l = "origin" end)
  module Modules = Label(struct let l = "modules" end)
  module Module_types = Label(struct let l = "module_types" end)
  module Name_f = Label(struct let l = "name" end)

  let (><) = Option.(><)

  let l = let open L in function | [] -> None | x -> Some x


  module Mu = struct
    let _m, module', arg = Schematic_indices.three
  end

  let named () = Schematic.pair String Mu.module'
  let dict () = Dict.sch Mu.module'
  let schr = Obj [
      Opt, Origin_f.l, (reopen Origin.sch);
      Opt, Modules.l,  dict ();
      Opt, Module_types.l, dict ()
    ]

  let d x = if x = Dict.empty then None else Some x
  let rec m = Custom { fwd; rev; sch = schr }
  and fwd x =
    let s = flatten x.signature in
    Record.[
      Origin_f.l $=? (default Origin.Submodule x.origin);
      Modules.l $=? d s.modules;
      Module_types.l $=? d s.module_types
    ]
  and rev = let open Record in
    fun [ _, o; _, m; _, mt] ->
      create ~origin:(o >< Origin.Submodule)
        (Exact { modules = m >< Dict.empty; module_types = mt >< Dict.empty})

  let opt_arg = option Mu.arg
  let rec module' =
    Custom { fwd = fwdm; rev=revm;
             sch = Sum[ "M", m;
                        "Alias", reopen Paths.S.sch;
                        "Fun", [opt_arg; Mu.module'];
                        "Abstract", reopen Id.sch;
                        "Link", reopen Paths.S.sch;
                        "Namespace", Array (named ())
                      ]
           }
  and fwdm = function
    | Sig m -> C (Z m)
    | Alias x -> C (S (Z (Namespaced.flatten x.path)))
    | Fun (arg,x) -> C (S (S (Z [arg;x])))
    | Abstract x -> C (S (S (S (Z x))))
    | Link x -> C (S (S (S (S (Z (Namespaced.flatten x))))))
    | Namespace n -> C (S (S (S (S (S (Z (to_list n)))))))
  and revm =
    let open Tuple in
    function
    | C Z m -> Sig m
    | C S Z  path -> Alias {path=Namespaced.of_path path; phantom=None}
    | C S S Z [arg;body] -> Fun(arg,body)
    | C S S S Z n -> Abstract n
    | C S S S S Z  path -> Link (Namespaced.of_path path)
    | C S S S S S Z modules ->
      Namespace (Dict.of_list modules)
    | _ -> .

  let arg = Arg.sch module'

  let defs : _ rec_defs = ["m", m; "module'", module'; "arg", arg]
  let m = Rec { id = ["Module"; "m"]; defs; proj = Zn }
  let module' = Rec { id = ["Module"; "module'"]; defs; proj = Sn Zn }

end

module Def = struct
  let empty = empty_sig
  let (|+>) m (name,x) = Name.Map.add name x m

  let modules dict = { empty with modules=dict }
  let merge = sig_merge

  let map f x = { modules = Name.Map.map f x.modules; module_types = Name.Map.map f x.module_types }

  let weak_merge (s1:definition) (s2:definition) =
    { module_types = Dict.weak_union s1.module_types s2.module_types;
      modules = Dict.weak_union s1.modules s2.modules }
  let add sg x = { sg with modules = sg.modules |+> x }
  let add_type sg x = { sg with module_types = sg.module_types |+> x }
  let add_gen level = match level with
    | Module -> add
    | Module_type -> add_type

  let find level name d = match level with
    | Module -> Name.Map.find_opt name d.modules
    | Module_type -> Name.Map.find_opt name d.module_types

  let remove level name d = match level with
    | Module ->
      let modules = Name.Map.remove name d.modules in
      { d with modules }
    | Module_type ->
      let module_types = Name.Map.remove name d.module_types in
      { d with module_types }

  let pp = pp_definition

  let sch = let open Schematic in let open Schema in
    let named = pair String module' in
    custom
      (Obj[Opt,Modules.l, Array named; Opt, Module_types.l, Array named])
      (fun x -> [ Modules.l $=? l(to_list x.modules);
                  Module_types.l $=? (l @@ to_list x.module_types)] )
      (let open Record in fun [_,m;_,mt] -> signature_of_lists (m><[]) (mt><[]))

  type t = definition
end


module Sig = struct

  let rec card s =
    let card_def s = let c= Name.Map.cardinal in
      c s.modules + c s.module_types in
    match s with
    | Blank -> 0
    | Divergence p ->
      card p.before + card_def p.after
    | Exact s -> card_def s

  let (|+>) m (name,x) = Name.Map.add name x m

  let rec gen_merge def_merge s1 s2 = match s1, s2 with
    | Blank, s | s, Blank -> s
    | Exact s1, Exact s2 ->  Exact (def_merge s1 s2)
    | Divergence p , Exact s ->
      Divergence { p with after = def_merge p.after s }
    | s, Divergence p ->
      Divergence { p with before = gen_merge def_merge s p.before }

  let merge x = gen_merge Def.merge x
  let weak_merge x = gen_merge Def.weak_merge x
  let diff = gen_merge sig_diff

  let flatten = flatten

  let create m = Exact { modules = empty |+> m; module_types = empty }
  let create_type m = Exact { module_types = empty |+> m; modules = empty }

  let is_exact = is_exact_sig

  let gen_create level md = match level with
    | Module -> create md
    | Module_type -> create_type md

  let of_lists l1 l2 = Exact (signature_of_lists l1 l2)
  let of_list ms =
    Exact { modules = List.fold_left (|+>) empty ms; module_types = empty }

    let of_list_type ms =
    Exact { module_types = List.fold_left (|+>) empty ms; modules = empty }


    let add_gen lvl sg x = match sg with
      | Blank -> Exact (Def.add_gen lvl Def.empty x)
      | Exact sg -> Exact (Def.add_gen lvl sg x)
      | Divergence p -> Divergence { p with after = Def.add_gen lvl p.after x }

    let add = add_gen Module
    let add_type = add_gen Module_type

  let empty = Exact Def.empty

  let pp = pp_signature

  type t = signature

  let sch = let open Schematic in let open Schema in
    let named = pair String module' in
    custom
      (Obj [Opt, Modules.l, Array named; Opt, Module_types.l, Array named])
      (fun x -> let s = flatten x in let l x = l(to_list x) in
        Record.[ Modules.l $=? l s.modules; Module_types.l $=? l s.module_types ])
      (let open Record in fun [_,m;_,mt] -> of_lists (m><[]) (mt><[]) )

end

let rec extend: type any. any ty -> extended ty = function
  | Abstract n -> Abstract n
  | Fun(a,x) ->
    let map a = Arg.map extend a in
    (Fun(Option.fmap map a, extend x) : modul_)
  | Sig s ->  Sig s
  | Alias _ as x -> x
  | Link _ as x -> x
  | Namespace _ as x -> x




module Subst = struct

  module Tbl = Support.Map.Make(Id)
  type 'x t = 'x ty Tbl.t
  type 'x subst = 'x t

  let identity = Tbl.empty
  let add id mty subst = Tbl.add id mty subst

  let rec apply: type any. (Id.t -> any ty option) -> any ty -> any ty = fun subst -> function
    | Abstract id as old -> Option.( subst id >< old)
    | Fun (x,y) -> Fun(Option.fmap (Arg.map (apply subst)) x, apply subst y)
    | Sig {origin;signature} ->
      Sig {origin;signature = apply_sig (fun id -> Option.fmap extend (subst id)) signature }
    | Alias _ as x -> x
    | Link _ as x -> x
    | Namespace _ as x -> x
  and apply_sig: (Id.t -> extended ty option) -> signature -> signature = fun subst -> function
    | Blank -> Blank
    | Exact s -> Exact (Def.map (apply subst) s)
    | Divergence d -> Divergence { point = d.point;
                        before = apply_sig subst d.before;
                        after = Def.map (apply subst) d.after
                      }

  let refresh seed x =
    let tbl = ref Tbl.empty in
    apply (fun k ->
        match Tbl.find_opt k !tbl with
        | Some _ as y -> y
        | None ->
          let fresh = Abstract (Id.create seed) in
          tbl := Tbl.add k fresh !tbl;
          Some fresh
      ) x

  let apply subst x = if subst = identity then x else
      apply (fun k -> Tbl.find_opt k subst) x

  let rec compute_constraints lvl (type any) (arg:any ty) (param:any ty) (subst: extended subst): extended subst =
    match arg, param with
    | x, Abstract id -> add id (extend x) subst
    | Fun _, Fun _  -> subst
    | Alias _, Alias _ -> subst
    | Link _, Link _  -> subst
    | Namespace _, Namespace _ -> subst
    | Sig arg, Sig param ->
      if lvl = Module then
        sig_constraints (Sig.flatten arg.signature) (Sig.flatten param.signature) subst
      else
        subst
    | _ -> (* type error *) subst
  and sig_constraints arg param subst =
    subst
    |> dict_constraints Module arg.modules param.modules
    |> dict_constraints Module_type arg.module_types param.module_types
  and dict_constraints lvl arg param subst =
    Name.Map.fold (fun k arg subst ->
        match Name.Map.find k param with
        | exception Not_found -> subst
        | param -> compute_constraints lvl arg param subst
      ) arg subst

  let rec replace_at ~level ~delete ~path ~replacement = function
    | Sig s ->
      let signature, subst = sig_replace_at ~level ~delete ~path ~replacement s.signature in
      Sig {s with signature}, subst
    | m -> (* type error *) m, identity
  and sig_replace_at ~level ~delete ~path ~replacement s =
    match path, s with
    | [], _ | _, Blank -> s, identity (* type error *)
    | _ :: _, Exact e ->
      let def, eq = def_replace_at ~level ~delete ~path ~replacement e in
      Exact def, eq
    | name :: q, Divergence ({after; before; _ } as d) ->
      let level' = match q with [] -> level | _ :: _ -> Module in
      match Def.find level' name after with
      | None ->
        let before, eq = sig_replace_at ~level ~delete ~path ~replacement before in
        Divergence ({ d with after; before }), eq
      | Some _ ->
        let after, eq = def_replace_at ~level ~delete ~path ~replacement after in
        Divergence ({ d with after; before }), eq
  and def_replace_at ~level ~delete ~path ~replacement s = match path with
    | [] -> (* error *) s, identity
    | [name] ->
      let s' =
        if delete then Def.remove level name s
        else Def.add_gen level s (name,extend replacement)
      in
      let eq =
        match Def.find level name s with
        | None -> (* type error *) identity
        | Some old ->
          compute_constraints level (extend replacement) old identity
      in
      s', eq
    | a :: q ->
      match Def.find Module a s with
      | None -> (* type error *) s, identity
      | Some sub ->
        let m, eq = replace_at ~level ~delete ~path:q ~replacement sub in
        Def.add_gen Module s (a,m), eq



  let compute_constraints ~arg ~param = compute_constraints Module arg param identity

  let pp ppf s =
    let pp_elt ppf (k,x) = Pp.fp ppf "%a->%a" Id.pp k pp x in
    Pp.list pp_elt ppf (Tbl.bindings s)

end

module Equal = struct

  type error_kind =
    | Kind
    | Alias of Namespaced.t * Namespaced.t
    | Alias_phantom
    | Abstract
    | Link of Namespaced.t * Namespaced.t
    | Origin of Origin.t * Origin.t
    | Divergence of Divergence.t * Divergence.t
    | Arg_name of string option * string option
    | Arg_kind
    | Signature_kind
    | Missing_item of Name.t

  let pp ppf = function
    | Kind -> Format.fprintf ppf "Mismatched kind"
    | Alias (x,y) ->
      Format.fprintf ppf "Mismatched alias: %a ≠ %a"
        Namespaced.pp x Namespaced.pp y
    | Alias_phantom ->
      Format.fprintf ppf "Mismatched alias: divergence point"
    | Link (x,y) ->
      Format.fprintf ppf "Mismatched link: %a ≠ %a"
        Namespaced.pp x Namespaced.pp y
    | Origin (x,y) ->
      Format.fprintf ppf "Mismatched origin: %a ≠ %a"
        Origin.pp x Origin.pp y
    | Arg_name (Some x, Some y) ->
      Format.fprintf ppf "Mismatched arg names: %s ≠ %s" x y
    | Arg_name (_,_) ->
      Format.fprintf ppf "Mismatched arg names: anonymous name"
    | Arg_kind -> Format.fprintf ppf "Mismatched generative functor arg"
    | Signature_kind -> Format.fprintf ppf "Mismatched signature kind"
    | Missing_item s -> Format.fprintf ppf "Missing item %s" s
    | Abstract -> Format.fprintf ppf "Mismatched abstract module types"
    | Divergence (x,y) ->
      Format.fprintf ppf "Mismatched divergence point: %a ≠ %a"
      Divergence.pp x
      Divergence.pp y


  let ok = Ok ()

   let (&&&) x y = match x, y with
     | Ok (), Ok () -> ok
     | Error _ as e, _ | _, (Error _ as e) -> e

  let rec eq: type a b. a ty -> b ty -> (unit,error_kind) result = fun x y ->
    match x, y with
    | Sig x, Sig y -> tsig x y
    | Alias x, Alias y ->
      if x.path = y.path then
        if x.phantom = y.phantom then ok
        else Error Alias_phantom
      else Error (Alias (x.path, y.path))
    | Abstract x, Abstract y ->
      if x = y then ok else Error Abstract
    | Fun (xa,xb), Fun (ya,yb) -> arg_opt xa ya &&& eq xb yb
    | Link x, (Link y) -> if x = y then ok else Error (Link (x,y))
    | Namespace x, Namespace y -> dict x y
    | _ -> Error Kind
  and tsig x y =
    (if x.origin = y.origin then ok else Error (Origin (x.origin,y.origin)))
    &&& signature x.signature y.signature
  and signature x y =
    match x, y with
    | Blank, Blank -> ok
    | Exact x, Exact y -> def x y
    | Divergence x, Divergence y ->
      (if x.point = y.point then ok else Error (Divergence (x.point,y.point)))
      &&& def x.after y.after
      &&& signature x.before y.before
    | _ -> Error Signature_kind
  and def x y =
    dict x.modules y.modules &&& dict x.module_types y.module_types
  and dict x y =
    let exception First of error_kind in
    match Name.Map.merge (fun k x y ->
        match x, y with
        | None, None -> None
        | Some _, None | None, Some _ -> raise (First (Missing_item k))
        | Some x, Some y ->
          match eq x y with
          | Ok () -> None
          | Error e -> raise (First e)
      ) x y
    with
    | _ -> ok
    | exception First e -> Error e
  and arg_opt: type a b. a ty Arg.t option -> b ty Arg.t option -> (unit,error_kind) result =
    fun x y -> match x, y with
      | Some x, Some y ->
        (if x.name = y.name then ok else Error (Arg_name (x.name, y.name))) &&& eq x.signature y.signature
      | None, None -> ok
      | _ -> Error Arg_kind
end

module Partial = struct

  type t = { name: string option; mty: sty }

  let empty_sig = { origin = Submodule; signature= Sig.empty}
  let empty = { name=None; mty = Sig empty_sig }
  let simple defs = { empty with mty = Sig { empty_sig with signature = defs} }
  let rec is_exact x = match x.mty with
      | Abstract _  -> true
      | Fun (_,x) -> is_exact {name=None; mty=x}
      | Sig s -> Sig.is_exact s.signature

  let rec to_module ?origin (p:t) =
    to_module_kind ?origin p.mty
  and to_module_kind ?origin : sty -> modul_ = function
    | Abstract n -> (Abstract n:modul_)
    | Fun(a,x) ->
      let map a = Arg.map (to_module_kind ?origin) a in
      (Fun(Option.fmap map a, to_module_kind ?origin x) : modul_)
    | Sig s ->
      let origin = match origin with
        | Some o -> Origin.at_most s.origin o
        | None -> s.origin
      in
      Sig {origin; signature = s.signature }


  let extend = extend
  let refresh = Subst.refresh
  let apply ~arg ~param ~body =
    let subst = Subst.compute_constraints ~arg ~param in
    Debug.debug "Constraint from typing:%a@." Subst.pp subst;
    let res = Subst.apply subst body in
    Debug.debug "Result:@ %a ⇒@ %a@." pp body pp res;
    res


  let replace_at ~level ~delete ~path ~replacement body =
    let constrained, eq = Subst.replace_at ~delete ~level ~path ~replacement body in
    Subst.apply eq constrained

 let rec pp_sty ppf: sty -> _ = function
    | Abstract n -> Pp.fp ppf "<abstract:%a>" Id.pp n
    | Fun (a,x) -> Pp.fp ppf "%a->%a" (Arg.pp pp_sty) a pp_sty x
    | Sig m -> pp_m ppf m

  let pp ppf (x:t) =
    let pp_name ppf = function
    | None -> ()
    | Some n -> Pp.fp ppf "(%s)" n in
    Pp.fp ppf "%a%a" pp_name x.name pp_sty x.mty



  let to_arg (p:t) = to_module p

  let rec of_extended_mty: modul_ -> sty = function
    | Abstract n -> Abstract n
    | Sig x -> Sig x
    | Fun (a,x) -> Fun(Option.fmap (Arg.map of_extended_mty) a, of_extended_mty x)
    | Link _ | Namespace _ | Alias _ -> assert false

  let of_extended ?name kind = { name; mty = of_extended_mty kind }

  let of_module name m =
    {name=Some name; mty = Sig m }

  let pseudo_module name x =
   let origin = Origin.Namespace in
   let signature =
     Exact {modules = x; module_types = Dict.empty } in
   of_module name { signature; origin }

   let is_functor x = match x.mty with
     | Fun _ -> true
     | _ -> false

   let to_sign fdefs = match fdefs.mty with
     | Abstract _ | Fun _ -> Error Sig.empty
     | Sig s ->
         Ok s.signature

   module Sch = struct
     open Schematic
     module S = Schema
     module Result = Label(struct let l = "signature" end)

     let mu = Schematic_indices.one

     let mty =
       custom
         (Sum ["Abstract", reopen Id.sch;
               "Sig", Schema.m;
               "Fun", [option @@ Arg.sch mu; mu];
              ])
         (fun (x:sty) -> match x with
            | Abstract x -> C (Z x)
            | Sig s -> C (S (Z s))
            | Fun (a,x) -> C (S (S (Z [a;x])))
            | _ -> .
         )
         (let open Tuple in
          function
           | C Z x -> Abstract x
           | C S Z x -> Sig x
           | C S S Z [a;x] -> Fun (a,x)
           | _ -> .
         )

     let mty = Rec { id = ["Partial"; "mty"]; defs=["mty", mty]; proj = Zn }


     let partial = custom [option String; mty]
         (fun {name; mty} -> [name;mty])
         (let open Tuple in fun [name;mty] -> {name;mty})
   end
   let sch = Sch.partial
 end

module Namespace = struct
  type t = dict
  let merge = Dict.union
  let merge_all = List.fold_left merge Dict.empty
  let rec from_module nms origin sign =
    match nms.Namespaced.namespace with
      | [] ->
        Dict.of_list [Modname.to_string @@ Unitname.modname nms.name, Sig (create ~origin sign)]
      | a :: namespace ->
        let sign = Namespace (from_module { nms with namespace } origin sign) in
        Dict.of_list [a, sign]

    let sch = Dict.sch Schema.module'
end
