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

module Precision = struct
  type t =
    | Exact
    | Unknown
  let sexp = let open Sexp in
    sum [ simple_constr "Exact" Exact; simple_constr "Unknown" Unknown]

  let reflect ppf = function
    | Exact -> Pp.fp ppf "Exact";
    | Unknown -> Pp.fp ppf "Unknwonw";

end

module Origin = struct
  type source = Pkg.t

  type t =
    | Unit of source (** aka toplevel module *)
    | Alias of t (** M = A… *)
    | Submodule
    | First_class (** Not resolved first-class module *)
    | Arg (** functor argument *)

  let rec pp ppf = function
    | Unit { Pkg.source= Local; _ } -> Pp.fp ppf "#"
    | Unit { Pkg.source = Pkg x; _ } -> Pp.fp ppf "#[%a]" Paths.Simple.pp x
    | Unit { Pkg.source = Unknown; _} -> Pp.fp ppf "#!"
    | Unit { Pkg.source = Special n; _} -> Pp.fp ppf "*(%s)" n
    | Submodule -> Pp.fp ppf "."
    | First_class -> Pp.fp ppf "'"
    | Arg -> Pp.fp ppf "§"
    | Alias n -> Pp.fp ppf "(≡%a)" pp n

  module Sexp = struct
    open Sexp
    let unit = C { name = "Unit";
                   proj = (function Unit s -> Some s | _ -> None);
                   inj = (fun x -> Unit x);
                   impl = Pkg.sexp;
                   default = None;
                 }

    let alias sexp = C { name = "Alias";
                         proj = (function Alias a -> Some a | _ -> None);
                         inj= (fun x -> Alias x);
                         impl = fix' sexp;
                         default = Some Submodule
                       }
    let submodule = simple_constr "Submodule" Submodule
    let first_class = simple_constr "First_class" First_class
    let arg = simple_constr "Arg" Arg


    let rec sexp () = Sexp.sum [unit;alias sexp; arg; submodule;first_class]
  end
  let sexp = Sexp.sexp ()


    let rec reflect ppf = function
    | Unit pkg  -> Pp.fp ppf "Unit %a" Pkg.reflect pkg
    | Submodule -> Pp.fp ppf "Submodule"
    | First_class -> Pp.fp ppf "First_class"
    | Arg -> Pp.fp ppf "Arg"
    | Alias n -> Pp.fp ppf {|Alias (%a)|} reflect n

  let at_most max v = match max, v with
    | (First_class|Arg| Alias _ ) , _ -> max
    | Unit _ , v -> v
    | Submodule, Unit _ -> Submodule
    | Submodule, Alias _ -> Submodule
    | Submodule, v -> v
end
type origin = Origin.t

type t = {
  name:Name.t;
  precision: Precision.t;
  origin: Origin.t;
  args: t option list;
  signature:signature;
}
and signature = { modules: mdict; module_types: mdict }
and mdict = t Name.Map.t

type arg = signature Arg.t
type modul = t

let of_arg ?(precision=Precision.Exact) ({name;signature}:arg) =
  { name; precision; origin = Arg ; args=[]; signature }

let is_functor = function
  | { args = []; _ } -> false
  |  _ -> true

type level = Module | Module_type

let pp_alias = Pp.opt Paths.Expr.pp

let pp_level ppf lvl =  Pp.fp ppf "%s" (match lvl with
    | Module -> "module"
    | Module_type -> "module type"
  )

let reflect_level ppf = function
    | Module -> Pp.string ppf "Module"
    | Module_type -> Pp.string ppf "Module type"

let rec reflect ppf {name;args;precision;origin;signature} =
  Pp.fp ppf {|@[<hov>{name="%s"; precision=%a; origin=%a; args=%a; signature=%a}@]|}
    name
    Precision.reflect precision
    Origin.reflect origin
    reflect_args args
    reflect_signature signature
and reflect_signature ppf {modules; module_types} =
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
and reflect_arg ppf arg = Pp.fp ppf "%a" (reflect_opt reflect) arg
and reflect_args ppf args =
  Pp.fp ppf "[%a]" (Pp.(list ~sep:(s "; @,") ) @@ reflect_arg ) args


let rec pp ppf {name;args;origin;signature;_} =
  Pp.fp ppf "%s%a:%a@[<hv>[@,%a@,]@]"
    name Origin.pp origin pp_args args pp_signature signature
and pp_signature ppf {modules; module_types} =
  Pp.fp ppf "@[<hv>%a" pp_mdict modules;
  if Name.Map.cardinal module_types >0 then
    Pp.fp ppf "@, __Types__:@, %a@]"
      pp_mdict module_types
  else Pp.fp ppf "@]"
and pp_mdict ppf dict =
  Pp.fp ppf "%a" (Pp.(list ~sep:(s " @,")) pp_pair) (Name.Map.bindings dict)
and pp_pair ppf (_,md) = pp ppf md
and pp_arg ppf arg = Pp.fp ppf "(%a)" (Pp.opt pp) arg
and pp_args ppf args = Pp.fp ppf "%a" (Pp.(list ~sep:(s "@,→") ) @@ pp_arg ) args;
    if List.length args > 0 then Pp.fp ppf "→"

let rec persistent ppf {name;args;signature;_} =
  Pp.fp ppf "@[<hov>(%s%a@,%a)@]"
    name
    pers_args args sig_persistent signature
and sig_persistent ppf {modules; module_types} =
  let card = Name.Map.cardinal in
  if card modules > 0 || card module_types > 0 then
    Pp.fp ppf "%a%a" (pers_mdict "modules") modules (pers_mdict "module types") module_types
  else
    ()
and pers_mdict kind ppf dict =
  if Name.Map.cardinal dict = 0 then ()
  else
    Pp.fp ppf "(%s %a)" kind (Pp.(list ~sep:(s " @,")) pers_pair) (Name.Map.bindings dict)
and pers_pair ppf (_,md) = persistent ppf md
and pers_arg ppf arg = Pp.fp ppf "%a" (Pp.opt persistent) arg
and pers_args ppf args =
  if args = [] then
    ()
  else
  Pp.fp ppf "(args %a)" (Pp.(list ~sep:(s "@,") ) @@ pers_arg ) args


let empty = Name.Map.empty

let create
    ?(args=[])
    ?(precision=Precision.Exact)
    ?(origin=Origin.Submodule) name signature =
  { name; origin; precision; args; signature}


let signature_of_lists ms mts =
  let e = Name.Map.empty in
  let add map m= Name.Map.add m.name m map in
  { modules = List.fold_left add e ms;
    module_types = List.fold_left add e mts
  }

module Sexp_core = struct
  open Sexp
  module R = Sexp.Record
  let to_list m = List.map snd @@ Name.Map.bindings m

  let args_f = U.(key Many "args" [])
  let modules = U.(key Many "modules" [])
  let module_types = U.(key Many "module_types" [])
  let origin = U.(key One_and_many "origin" Origin.Submodule)
  let precision = U.(key One_and_many "precision" Precision.Exact)

  let rec module_ () =
    let fr r =
      r.name, R.(create [
                 field precision r.precision;
                 field origin r.origin;
                 field args_f r.args;
                 field modules (to_list r.signature.modules);
                 field module_types (to_list r.signature.module_types)
                ]) in
    let f (name,x) =
      let get f = R.get f x in
      create ~origin:(get origin) ~precision:(get precision) ~args:(get args_f)
        name
      @@ signature_of_lists (get modules) (get module_types) in
    let record =
      record [
        field precision Precision.sexp;
        field origin Origin.sexp;
        field args_f @@ fix args;
        field modules @@ list @@ fix module_;
        field module_types @@ list @@ fix module_
      ]
    in
    conv {f;fr} (pair' string record)
  and args () = list @@ opt @@ fix module_

  let modul_ = module_ ()
end
let sexp = Sexp_core.modul_

module Sig = struct

  let card s =
    let card = Name.Map.cardinal in
    card s.modules + card s.module_types

  let persistent = sig_persistent

  let (|+>) m x = Name.Map.add x.name x m

  let merge s1 s2 =
  { modules = Name.Map.union' s1.modules s2.modules
  ; module_types = Name.Map.union' s1.module_types s2.module_types
  }

  let create m = { modules = empty |+> m; module_types = empty }
  let create_type m = { module_types = empty |+> m; modules = empty }

  let gen_create level md = match level with
    | Module -> create md
    | Module_type -> create_type md

  let of_lists = signature_of_lists
  let of_list ms =
    { modules = List.fold_left (|+>) empty ms; module_types = empty }

    let of_list_type ms =
    { module_types = List.fold_left (|+>) empty ms; modules = empty }

  let add sg x = { sg with modules = sg.modules |+> x }
  let add_type sg x = { sg with module_types = sg.module_types |+> x }
  let add_gen level = match level with
    | Module -> add
    | Module_type -> add_type

  let empty =
    {modules = empty; module_types = empty }

  let pp = pp_signature

  type t = signature

  let sexp =
    let open Sexp in
    let open Sexp_core in
    let r = record [ field modules @@ list @@ fix module_;
                     field module_types @@ list @@ fix module_
                   ] in
    let f x =
      of_lists (R.get modules x) (R.get module_types x)
    in
    let fr s =
      R.(create [field modules @@ to_list s.modules;
                 field module_types @@ to_list s.module_types] ) in
    convr r f fr

end

module Partial = struct
  type nonrec t =
    { precision: Precision.t;
      origin: Origin.t;
      args: t option list;
      result: signature }
  let empty = { origin = Submodule; precision=Exact; args = []; result= Sig.empty }
  let simple defs = { empty with result = defs }

  let pp ppf (x:t) =
    if x.args = [] then
      Pp.fp ppf "%a(%a)" pp_signature x.result Origin.pp x.origin
    else Pp.fp ppf "%a@,→%a(%a)"
        pp_args x.args
        pp_signature x.result
        Origin.pp x.origin

  let no_arg x = { origin = Submodule; precision=Exact; args = []; result = x }

  let drop_arg (p:t) = match  p.args with
    | _ :: args -> Some { p with args }
    | [] -> None

  let to_module ?origin name (p:t) =
    let origin = match origin with
      | Some o -> Origin.at_most p.origin o
      | None -> p.origin
    in
    {name;origin; precision = p.precision; args = p.args; signature = p.result }

  let to_arg name (p:t) =
    {name;
     origin = p.origin;
     precision = p.precision;
     args = p.args;
     signature = p.result }

  let of_module {args;signature;origin; precision; _} =
    {precision;origin;result=signature;args}

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
    let ksign = U.(key Many "signature" Sig.empty)
    let partial =
      let r = record [ field ksign Sig.sexp; field C.args_f @@ C.args () ] in
      let f x = {args = R.get C.args_f x; result = R.get ksign x;
                 precision=Precision.Exact; origin = Submodule
                } in
      let fr r = R.(create [field ksign r.result; field C.args_f r.args] ) in
      conv {f;fr} r

  end
  let sexp = Sexp.partial


end
