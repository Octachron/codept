module Pkg = Paths.Pkg

module Arg = struct
  type 'a t = { name:Name.t; signature:'a }
  type 'a arg = 'a t

  let pp pp ppf = function
    | Some arg ->
      Pp.fp ppf "(%s:%a)" arg.name pp arg.signature
    | None -> Pp.fp ppf "()"

  let sexp sign =
    Sexp.convr
      (Sexp.pair Sexp.string sign)
      (fun (x,y) -> {name=x; signature=y} )
      ( fun r -> r.name, r.signature )

  let reflect pp ppf = function
    | Some arg ->
      Pp.fp ppf {|Some {name="%s"; %a}|} arg.name pp arg.signature
    | None -> Pp.fp ppf "()"

  let pp_s pp_sig ppf args = Pp.fp ppf "%a"
      (Pp.(list ~sep:(s "→@,")) @@ pp pp_sig) args;
    if List.length args > 0 then Pp.fp ppf "→"
end


module Divergence= struct
  type kind =
    | First_class_module
    | External
  type t = Name.t *  kind * (Paths.Pkg.t * Loc.t)

  let pp_kind ppf s =
    Pp.fp ppf "%s" @@
    match s with
    | First_class_module -> "First class module"
    | External -> "External"

  let pp ppf (name, kind, (path,loc) ) =
    Pp.fp ppf "%a%a:%s divergence:%a"
      Paths.Pkg.pp path Loc.pp loc
      name
      pp_kind kind

end


module Origin = struct
  type source = Pkg.t

  type t =
    | Unit of source (** aka toplevel module *)
    | Submodule
    | First_class (** Not resolved first-class module *)
    | Arg (** functor argument *)

  let pp ppf = function
    | Unit { Pkg.source= Local; _ } -> Pp.fp ppf "#"
    | Unit { Pkg.source = Pkg x; _ } -> Pp.fp ppf "#[%a]" Paths.Simple.pp x
    | Unit { Pkg.source = Unknown; _} -> Pp.fp ppf "#!"
    | Unit { Pkg.source = Special n; _} -> Pp.fp ppf "*(%s)" n
    | Submodule -> Pp.fp ppf "."
    | First_class -> Pp.fp ppf "'"
    | Arg -> Pp.fp ppf "§"

  module Sexp = struct
    open Sexp
    let unit = C { name = "Unit";
                   proj = (function Unit s -> Some s | _ -> None);
                   inj = (fun x -> Unit x);
                   impl = Pkg.sexp;
                   default = None;
                 }

    let submodule = simple_constr "Submodule" Submodule
    let first_class = simple_constr "First_class" First_class
    let arg = simple_constr "Arg" Arg


    let  sexp = Sexp.sum [unit; arg; submodule;first_class]
  end
  let sexp = Sexp.sexp


    let reflect ppf = function
    | Unit pkg  -> Pp.fp ppf "Unit %a" Pkg.reflect pkg
    | Submodule -> Pp.fp ppf "Submodule"
    | First_class -> Pp.fp ppf "First_class"
    | Arg -> Pp.fp ppf "Arg"

  let at_most max v = match max, v with
    | (First_class|Arg ) , _ -> max
    | Unit _ , v -> v
    | Submodule, Unit _ -> Submodule
    | Submodule, v -> v
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
  | Alias of { name:Name.t; path: Paths.S.t; exact:bool }

and definition = { modules : mdict; module_types : mdict }
and signature =
  | Blank
  | Exact of definition
  | Divergence of { point: Divergence.t; before:signature; after:definition}
and mdict = t Name.Map.t

type arg = definition Arg.t
type modul_ = t

let of_arg ({name;signature}:arg) =
  { name; origin = Arg ; args=[]; signature = Exact signature }

let is_functor = function
  | Alias _ |  M { args = []; _ } -> false
  |  _ -> true

let name = function
  | Alias {name; _ } -> name
  | M {name; _ } -> name


let sig_merge s1 s2 =
  { modules = Name.Map.union' s1.modules s2.modules
  ; module_types = Name.Map.union' s1.module_types s2.module_types
  }

let empty = Name.Map.empty
let empty_sig = {modules = empty; module_types = empty }

let rec flatten = function
  | Exact x -> x
  | Divergence d -> sig_merge (flatten d.before) d.after
  | Blank -> empty_sig

let is_exact m =
  match m with
  | Alias {exact; _ } -> exact
  | M m ->
    match m.signature with
    | Exact _ -> true
    | Divergence _ -> false
    | Blank -> false

let md s = M s

let rec aliases0 l = function
  | Alias {path = a :: _ ; _ } -> a :: l
  | Alias _ -> l
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

let reflect_level ppf = function
    | Module -> Pp.string ppf "Module"
    | Module_type -> Pp.string ppf "Module type"

let rec reflect ppf = function
  | M m ->  Pp.fp ppf "M %a" reflect_m m
  | Alias {name;path;exact} -> Pp.fp ppf "Alias {name=%a;path=[%a];exact=%s}"
                                 Pp.estring name
                                 Pp.(list ~sep:(s ";@  ") @@ estring ) path
                                 (if exact then "true" else "false")
and reflect_m ppf {name;args;origin;signature} =
  Pp.fp ppf {|@[<hov>{name="%s"; origin=%a; args=%a; signature=Exact(%a)}@]|}
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

let rec pp ppf = function
  | Alias {name;path;exact} ->
    Pp.fp ppf "%s≡%s%a" name (if exact then "" else "(?)" ) Paths.S.pp path
  | M m -> pp_m ppf m
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
    | _, Some p -> Origin.Unit p
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


let signature_of_lists ms mts =
  let e = Name.Map.empty in
  let add map m= Name.Map.add (name m) m map in
  { modules = List.fold_left add e ms;
    module_types = List.fold_left add e mts
  }

module Sexp_core = struct
  open Sexp
  module R = Sexp.Record
  let to_list m = List.map snd @@ Name.Map.bindings m

  let args_f = R.(key Many "args" [])
  let modules = R.(key Many "modules" [])
  let module_types = R.(key Many "module_types" [])
  let origin = R.(key One_and_many "origin" Origin.Submodule)

  let rec m () =
    let fr r =
      r.name,
      let signature = flatten r.signature in
      R.(create [
                 origin := r.origin;
                 args_f := r.args;
                 modules := (to_list signature.modules);
                 module_types := (to_list signature.module_types)
                ]) in
    let f (name,x) =
      let get f = R.get f x in
      create ~origin:(get origin) ~args:(get args_f)
        name
      @@ Exact (signature_of_lists (get modules) (get module_types)) in
    let record =
      record R.[
        field origin Origin.sexp;
        field args_f @@ fix args;
        field modules @@ list @@ fix' module_;
        field module_types @@ list @@ fix' module_
      ]
    in
    conv {f;fr} (key_list string record)
  and args () = list @@ opt @@ fix' m
  and module_ () =
    let alias = C2.C { name = "Alias";
                       proj = (function Alias a -> Some(a.name, a.path)
                                      | _ -> None);
                    inj = (fun (name,path) -> Alias {name;path;exact=true} );
                    impl = key_list string Paths.S.sexp;
                     } in
    let mc =
      C2.C {name = "M";
         proj = (function M m -> Some m | _ -> None );
         inj = (fun m -> M m);
         impl = fix' m;
        } in
    C2.sum mc [alias; mc ]


  let modul_ = module_ ()
end
let sexp = Sexp_core.modul_

module Def = struct
  let empty = empty_sig
  let (|+>) m x = Name.Map.add (name x) x m

  let merge = sig_merge
  let add sg x = { sg with modules = sg.modules |+> x }
  let add_type sg x = { sg with module_types = sg.module_types |+> x }
  let add_gen level = match level with
    | Module -> add
    | Module_type -> add_type


  let sexp =
    let open Sexp in
    let open Sexp_core in
    let r = record R.[ field modules @@ list @@ fix' module_;
                       field module_types @@ list @@ fix' module_
                     ] in
    let f x =
      signature_of_lists (R.get modules x) (R.get module_types x)
    in
    let fr s =
      R.(create [ modules := to_list s.modules;
                  module_types := to_list s.module_types] )
    in
    convr r f fr

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
      | Blank -> Blank
      | Exact sg -> Exact (Def.add_gen lvl sg x)
      | Divergence p -> Divergence { p with after = Def.add_gen lvl p.after x }

    let add = add_gen Module
    let add_type = add_gen Module_type

  let empty = Exact Def.empty

  let pp = pp_signature

  type t = signature

  let sexp =
    let open Sexp in
    let open Sexp_core in
    let r = record R.[ field modules @@ list @@ fix' module_;
                     field module_types @@ list @@ fix' module_
                   ] in
    let f x =
      of_lists (R.get modules x) (R.get module_types x)
    in
    let fr s =
      let s = flatten s in
      R.(create [ modules := to_list s.modules;
module_types := to_list s.module_types] )
in
    convr r f fr

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

  module Sexp = struct
    open Sexp
    module C = Sexp_core
    module R = Sexp.Record
    let ksign = R.(key Many "signature" Def.empty)
    let partial =
      let r = record R.[
          field C.origin Origin.sexp;
          field ksign Def.sexp;
          field C.args_f @@ C.args ();
        ] in
      let f x =
        let get f = R.get f x in
        {args = get C.args_f; result = Exact (get ksign);
         origin = get C.origin }
        in
let fr r = R.(create [ksign := flatten r.result;
                      C.args_f := r.args;
                      C.origin := r.origin
                     ] ) in
      conv {f;fr} r

  end
  let sexp = Sexp.partial


end
