module Pkg = Paths.Pkg

module Arg = struct
  type 'a t = { name:Name.t; signature:'a }
  type 'a arg = 'a t

  let pp pp ppf = function
    | Some arg ->
      Pp.fp ppf "(%s:%a)" arg.name pp arg.signature
    | None -> Pp.fp ppf "()"

  let sch name sign = let open Schematic.Tuple in
    let fwd arg = [arg.name; arg.signature] in
    let rev [name;signature] = {name;signature} in
    Schematic.custom ["Module"; "Arg"; name] Schematic.[String; sign] fwd rev

  let reflect pp ppf = function
    | Some arg ->
      Pp.fp ppf {|Some {name="%s"; %a}|} arg.name pp arg.signature
    | None -> Pp.fp ppf "()"

  let pp_s pp_sig ppf args = Pp.fp ppf "%a"
      (Pp.(list ~sep:(s "→@,")) @@ pp pp_sig) args;
    if List.length args > 0 then Pp.fp ppf "→"
end


module Divergence= struct
  type origin =
    | First_class_module
    | External
  type t =  { root: Name.t; origin:origin; loc: Paths.Pkg.t * Loc.t }

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

  let floc =
    Pp.decorate "(" ")" @@ Pp.pair Paths.P.reflect rloc

  let divergence ppf {root;loc;origin} =
    Pp.fp ppf "{root=%a;loc=%a;origin=%a}" Pp.estring root floc loc origin_r origin

  end
  let reflect = Reflect.divergence

  let pp ppf {root; origin; loc= (path,loc) } =
    Pp.fp ppf "open %s at %a:%a (%a)"
      root
      Paths.Pkg.pp path Loc.pp loc
      pp_origin origin

  let sch_origin =
    let open Schematic in
    custom ["Module"; "Divergence"; "origin" ]
      (Sum[ "First_class_module",  Void; "External", Void])
      (function
        | First_class_module -> C E
        | External -> C (S E))
      (function
      | C E -> First_class_module
      | C (S E) -> External
      | _ -> .)

  let sch = let open Schematic in let open Tuple in
    custom ["Module"; "Divergence"; "t"]
      Schematic.[String; sch_origin; [Paths.P.sch; Loc.Sch.t ]]
      (fun r -> [r.root;r.origin; [fst r.loc; snd r.loc] ])
      (fun [root;origin;[s;l]] -> {root;origin;loc=(s,l)} )

end


module Origin = struct

  type t =
    | Unit of {source:Paths.P.t; path:Paths.S.t}
    (** aka toplevel module *)
    | Submodule
    | First_class (** Not resolved first-class module *)
    | Arg (** functor argument *)
    | Phantom of bool * Divergence.t
    (** Ambiguous module, that could be an external module *)

  let pp ppf = function
    | Unit s ->
      begin match s.source.Pkg.source with
        | Pkg.Local-> Pp.fp ppf "#"
        | Pkg x -> Pp.fp ppf "#[%a]" Paths.Simple.pp x
        | Unknown -> Pp.fp ppf "#!"
        | Special n -> Pp.fp ppf "*(%s)" n
      end
    | Submodule -> Pp.fp ppf "."
    | First_class -> Pp.fp ppf "'"
    | Arg -> Pp.fp ppf "§"
    | Phantom _ -> Pp.fp ppf "👻"

  module Sch = struct open Schematic
    let raw =
      Sum [ "Unit", [Paths.P.sch; Paths.S.sch];
            "Submodule", Void; "First_class", Void; "Arg", Void;
            "Phantom", [ Bool; Divergence.sch]]
    let t = let open Tuple in
      custom ["Module"; "Origin"; "t"] raw
        (function
          | Unit {source; path} -> C (Z [source;path])
          | Submodule -> C (S E)
          | First_class -> C (S (S E))
          | Arg -> C(S (S (S E)))
          | Phantom (b,div) -> C (S (S (S (S(Z [b;div])))))
        )
        (function
          | C Z [source;path] -> Unit {source;path}
          | C S E -> Submodule
          | C S S E -> First_class
          | C S S S E -> Arg
          | C S S S S Z [b;d] -> Phantom(b,d)
          | _ -> .
        )
  end let sch = Sch.t

    let reflect ppf = function
      | Unit u  -> Pp.fp ppf "Unit {source=%a;path=[%a]}"
                     Pkg.reflect u.source Pp.(list ~sep:(const ";@ ") estring) u.path
    | Submodule -> Pp.fp ppf "Submodule"
    | First_class -> Pp.fp ppf "First_class"
    | Arg -> Pp.fp ppf "Arg"
    | Phantom (root,b) -> Pp.fp ppf "Phantom (%b,%a)" root Divergence.reflect b


  let at_most max v = match max, v with
    | (First_class|Arg ) , _ -> max
    | Unit _ , v -> v
    | Submodule, Unit _ -> Submodule
    | Phantom _, Submodule -> Submodule
    | Submodule, v -> v
    | Phantom _ as ph , _ -> ph
end
type origin = Origin.t

type m = {
      name:Name.t;
      origin: Origin.t;
      args: m option list;
      signature:signature;
    }
and t =
  | M of m
  | Alias of
      { name:Name.t;
        path:Namespaced.t;
        phantom: Divergence.t option;
        weak:bool }
  | Namespace of {name: Name.t; modules: dict}
and definition = { modules : dict; module_types : dict }
and signature =
  | Blank
  | Exact of definition
  | Divergence of { point: Divergence.t; before:signature; after:definition}
and dict = t Name.Map.t

type arg = definition Arg.t
type modul_ = t

let of_arg ({name;signature}:arg) =
  { name; origin = Arg ; args=[]; signature = Exact signature }

let is_functor = function
  | Alias _ |  M { args = []; _ } -> false
  |  M _ | Namespace _ -> true

let name = function
  | Alias {name; _ } -> name
  | M {name; _ } -> name
  | Namespace {name; _ } -> name


module Dict = struct
  type t = dict
  let empty = Name.Map.empty
  let of_list = List.fold_left (fun x m -> Name.Map.add (name m) m x) empty

  let union =
    let rec merge _k x y = match x, y with
      | (M { origin = Unit {path = p;_}; _ } as x), Alias {weak=true; path; _ }
        when Namespaced.flatten path = p -> Some x
      (*      | x, Alias {weak=true; _ } -> Some x *)
      | Namespace n, Namespace n' ->
        Some (
          Namespace { name = n.name;
                      modules = Name.Map.union merge n.modules n'.modules
                    }
        )
      | _, r -> Some r in
    Name.Map.union merge

end

(* TODO: Behavior with weak aliases *)
let rec spirit_away breakpoint root = function
  | Alias a as al ->
    if not root then
      Alias { a with phantom = Some breakpoint }
    else al
  | Namespace {name; modules } ->
    Namespace {name;
               modules = Name.Map.map (spirit_away breakpoint false) modules }
  | M m ->
    let origin = Origin.Phantom (root,breakpoint) in
    let origin = match m.origin with
      | Unit _  as u -> u
      | Phantom _ as ph -> ph
      | _ -> origin in
    M { m with origin;
               signature = spirit_away_sign breakpoint false m.signature }
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

let sig_merge (s1:definition) (s2:definition) =
  { module_types = Name.Map.union' s1.module_types s2.module_types;
    modules = Dict.union s1.modules s2.modules }

let empty = Name.Map.empty
let empty_sig = {modules = empty; module_types = empty }

let rec flatten = function
  | Exact x -> x
  | Divergence d -> sig_merge (flatten d.before) d.after
  | Blank -> empty_sig

let is_exact m =
  match m with
  | Namespace _ -> true
  | Alias {phantom ; _ } -> phantom = None
  | M m ->
    match m.signature with
    | Exact _ -> true
    | Divergence _ -> false
    | Blank -> false

let md s = M s

let rec aliases0 l = function
  | Alias {path; _ } -> path :: l
  | Namespace {modules; _ } ->
      Name.Map.fold (fun _ x l -> aliases0 l x) modules l
  | M { signature; _ } ->
    let signature = flatten signature in
    let add _k x l = aliases0 l x in
    Name.Map.fold add signature.modules
    @@ Name.Map.fold add signature.module_types
    @@ []

let aliases = aliases0 []

type level = Module | Module_type

let pp_alias = Pp.opt Paths.Expr.pp

let pp_level ppf lvl =  Pp.fp ppf "%s" (match lvl with
    | Module -> "module"
    | Module_type -> "module type"
  )

let reflect_phantom ppf = function
  | None -> Pp.fp ppf "None"
  | Some x -> Pp.fp ppf "Some(%a)" Divergence.reflect x

let rec reflect ppf = function
  | M m ->  Pp.fp ppf "M %a" reflect_m m
  | Namespace {name; modules} ->
    Pp.fp ppf "Namespace {name=%a; modules=%a}"
      Pp.estring name reflect_mdict modules
  |  Alias {name;path;phantom;weak} ->
    Pp.fp ppf "Alias {name=%a;path=[%a];phantom=%a;weak=%b}"
      Pp.estring name
      reflect_namespaced path
      reflect_phantom phantom
      weak
and reflect_namespaced ppf nd =
  if nd.namespace = [] then
    Pp.fp ppf "Namespaced.make %a"
      Pp.estring nd.name
  else
    Pp.fp ppf "Namespaced.make ~namespace:(%a) %a"
      Pp.(list ~sep:(s";@ ") @@ estring) nd.namespace
      Pp.estring nd.name
and reflect_m ppf {name;args;origin;signature} =
  Pp.fp ppf {|@[<hov>{name="%s"; origin=%a; args=%a; signature=%a}@]|}
    name
    Origin.reflect origin
    reflect_args args
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
      Pp.(list ~sep:(s "; @,") @@ reflect_pair) ppf (Name.Map.bindings dict)
and reflect_pair ppf (_,md) = reflect ppf md
and reflect_opt reflect ppf = function
  | None -> Pp.string ppf "None"
  | Some x -> Pp.fp ppf "Some %a" reflect x
and reflect_arg ppf arg = Pp.fp ppf "%a" (reflect_opt reflect_m) arg
and reflect_args ppf args =
  Pp.fp ppf "[%a]" (Pp.(list ~sep:(s "; @,") ) @@ reflect_arg ) args

let reflect_modules ppf dict =
  Pp.fp ppf "Dict.of_list @[<v 2>[%a]@]"
    (Pp.list ~sep:(Pp.s "; @,") @@ fun ppf (_,m) -> reflect ppf m)
    (Name.Map.bindings dict)

let rec pp ppf = function
  | Alias {name;path;phantom;weak=_} ->
    Pp.fp ppf "%s≡%s%a" name (if phantom=None then "" else "(👻)" )
      Namespaced.pp path
  | M m -> pp_m ppf m
  | Namespace n -> Pp.fp ppf "Namespace %s=@[[%a]@]" n.name
                     pp_mdict n.modules
and pp_m ppf {name;args;origin;signature;_} =
  Pp.fp ppf "%s%a:%a@[<hv>[@,%a@,]@]"
    name Origin.pp origin pp_args args pp_signature signature
and pp_signature ppf = function
  | Blank -> Pp.fp ppf "ø"
  | Exact s -> pp_definition ppf s
  | Divergence {point; before; after} ->
      Pp.fp ppf "%a ∘ %a ∘ %a"
      pp_signature before
      Divergence.pp point
      pp_definition after
and pp_definition ppf {modules; module_types} =
  Pp.fp ppf "@[<hv>%a" pp_mdict modules;
  if Name.Map.cardinal module_types >0 then
    Pp.fp ppf "@, __Types__:@, %a@]"
      pp_mdict module_types
  else Pp.fp ppf "@]"
and pp_mdict ppf dict =
  Pp.fp ppf "%a" (Pp.(list ~sep:(s " @,")) pp_pair) (Name.Map.bindings dict)
and pp_pair ppf (_,md) = pp ppf md
and pp_arg ppf arg = Pp.fp ppf "(%a)" (Pp.opt pp_m) arg
and pp_args ppf args = Pp.fp ppf "%a" (Pp.(list ~sep:(s "@,→") ) @@ pp_arg )
    args;
    if List.length args > 0 then Pp.fp ppf "→"




let mockup ?origin ?path name =
  let origin = match origin, path with
    | _, Some p -> Origin.Unit {source= p; path=[name]}
    | Some o, None -> o
    | _ -> Submodule in
  {
    name;
    origin;
    args = [];
    signature = Blank
  }

let create
    ?(args=[])
    ?(origin=Origin.Submodule) name signature =
  { name; origin; args; signature}

let namespace (path:Namespaced.t) =
  let rec namespace (global:Namespaced.t) path =
    match path with
    | [] -> raise (Invalid_argument "Module.namespace: empty namespace")
    | [name] ->
      let placeholder =
        Alias { name= global.name; path=global; phantom = None; weak = true } in
      Namespace { name; modules = Dict.of_list[placeholder] }
  | name :: rest ->
    Namespace {name; modules = Dict.of_list [namespace global rest] }
  in
  namespace path path.namespace

let rec with_namespace nms module'=
  match nms with
  | [] -> module'
  | a :: q ->
    let sub = with_namespace q module' in
    Namespace { name = a; modules = Dict.of_list [sub] }

let signature_of_lists ms mts =
  let e = Name.Map.empty in
  let add map m= Name.Map.add (name m) m map in
  { modules = List.fold_left add e ms;
    module_types = List.fold_left add e mts
  }

let to_list m = List.map snd @@ Name.Map.bindings m

module Schema = struct
  open Schematic
  module Origin_f = Label(struct let l = "origin" end)
  module Args = Label(struct let l = "args" end)
  module Modules = Label(struct let l = "modules" end)
  module Module_types = Label(struct let l = "module_types" end)
  module Name_f = Label(struct let l = "name" end)

  let (><) = Option.(><)

  let l = let open L in function | [] -> None | x -> Some x

  let rec schr = Obj [
      Req, Name_f.l, String;
      Opt, Origin_f.l, Origin.sch;
      Opt, Args.l, args;
      Opt, Modules.l, Array module';
      Opt, Module_types.l, Array module'
    ]
  and opt_m =
    Custom { fwd=ofwd ; rev = orev ; sch= Sum ["None",Void;"Some", m];
             id=["Module"; "Option"; "m"] }
  and args = Array opt_m
  and ofwd: m option -> 'a = function None -> C E | Some x -> C(S(Z x))
  and orev: 'a -> m option = function C E -> None | C S Z x-> Some x | _ -> .
  and m = Custom { fwd; rev; sch = schr; id = ["Module"; "m"] }
  and fwd x =
    let s = flatten x.signature in
    Record.[
      Name_f.l $= x.name;
      Origin_f.l $=? (default Origin.Submodule x.origin);
      Args.l $=? (l x.args);
      Modules.l $=? (l @@ to_list s.modules);
      Module_types.l $=? (l @@ to_list s.module_types)
    ]
  and rev = let open Record in
    fun [_, name; _, o; _, a; _, m; _, mt] ->
      create ~args:(a><[]) ~origin:(o >< Origin.Submodule) name
        (Exact (signature_of_lists (m >< []) (mt >< [])) )
  and module' =
    Custom { fwd = fwdm; rev=revm;
             sch = Sum[ "M", m; "Alias", [String;Paths.S.sch];
                        "Namespace", [String; Array module']];
             id = ["Module"; "module"] }
  and fwdm = function
    | M m -> C (Z m)
    | Alias x -> C (S (Z (Tuple.[x.name; Namespaced.flatten x.path ])))
    | Namespace n -> C (S (S (Z ( Tuple.[n.name; to_list n.modules] ))))
  and revm = let open Tuple in
    function
    | C Z m -> M m
    | C S Z  [name;path] -> Alias {name; path=Namespaced.of_path path;
                                   phantom=None; weak = false}
    | C S S Z [name; modules] ->
      Namespace { name; modules = Dict.of_list modules }
    | _ -> .

end

module Def = struct
  let empty = empty_sig
  let (|+>) m x = Name.Map.add (name x) x m

  let modules dict = { empty with modules=dict }
  let merge = sig_merge
  let add sg x = { sg with modules = sg.modules |+> x }
  let add_type sg x = { sg with module_types = sg.module_types |+> x }
  let add_gen level = match level with
    | Module -> add
    | Module_type -> add_type

  let pp = pp_definition

  let sch = let open Schematic in let open Schema in
    custom ["Module"; "Def"; "t"]
      (Obj[Opt,Modules.l, Array module'; Opt, Module_types.l, Array module'])
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

  let (|+>) m x = Name.Map.add (name x) x m

  let rec merge s1 s2 = match s1, s2 with
    | Blank, s | s, Blank -> s
    | Exact s1, Exact s2 ->  Exact (Def.merge s1 s2)
    | Divergence p , Exact s ->
      Divergence { p with after = Def.merge p.after s }
    | s, Divergence p ->
      Divergence { p with before = merge s p.before }

  let flatten = flatten

  let create m = Exact { modules = empty |+> m; module_types = empty }
  let create_type m = Exact { module_types = empty |+> m; modules = empty }

  let is_exact = function
    | Exact _ -> true
    | Divergence _ | Blank -> false

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
    custom ["Module"; "signature"]
      (Obj [Opt, Modules.l, Array module'; Opt, Module_types.l, Array module'])
      (fun x -> let s = flatten x in let l x = l(to_list x) in
        Record.[ Modules.l $=? l s.modules; Module_types.l $=? l s.module_types ])
      (let open Record in fun [_,m;_,mt] -> of_lists (m><[]) (mt><[]) )

end


module Partial = struct
  type nonrec t =
    { origin: Origin.t;
      args: m option list;
      result: signature }
  let empty = { origin = Submodule; args = []; result= Sig.empty }
  let simple defs = { empty with result = defs }
  let is_exact x = Sig.is_exact x.result

  let pp ppf (x:t) =
    if x.args = [] then
      Pp.fp ppf "%a(%a)" pp_signature x.result Origin.pp x.origin
    else Pp.fp ppf "%a@,→%a(%a)"
        pp_args x.args
        pp_signature x.result
        Origin.pp x.origin

  let no_arg x = { origin = Submodule; args = []; result = x }

  let drop_arg (p:t) = match  p.args with
    | _ :: args -> Some { p with args }
    | [] -> None

  let to_module ?origin name (p:t) =
    let origin = match origin with
      | Some o -> Origin.at_most p.origin o
      | None -> p.origin
    in
    {name;origin; args = p.args; signature = p.result }

  let to_arg name (p:t) =
    {name;
     origin = p.origin;
     args = p.args;
     signature = p.result }

  let of_module {args;signature;origin; _} =
    {origin;result=signature;args}

  let is_functor x = x.args <> []

  let to_sign fdefs =
    if fdefs.args <> [] then
      Error fdefs.result

    else
      Ok fdefs.result

  module Sch = struct
    open Schematic
    module S = Schema
    module Result = Label(struct let l = "signature" end)
    let raw =
      Obj [ Opt, S.Origin_f.l, Origin.sch;
            Opt, S.Args.l, S.args;
            Opt, Result.l, Def.sch;
          ]

    let (><) = Option.(><)
    let partial = custom ["Module"; "partial"] raw
        (fun {args;origin;result} ->
           Record.[ S.Origin_f.l $=? default Origin.Submodule origin;
                    S.Args.l $=? (S.l args);
                    Result.l $=? default Def.empty (flatten result);
                  ])
        (let open Record in fun [_,origin; _, args;_,result] ->
            { args = args >< []; origin = origin >< Submodule;
              result = Exact(result>< Def.empty) }
        )
  end let sch = Sch.partial

end
