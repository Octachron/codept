module Arg = struct
  type 'a t = { name:Name.t; signature:'a }
  type 'a arg = 'a t

  let pp pp ppf = function
    | Some arg ->
      Pp.fp ppf "(%s:%a)" arg.name pp arg.signature
    | None -> Pp.fp ppf "()"

  let reflect pp ppf = function
    | Some arg ->
      Pp.fp ppf {|Some {name="%s"; %a}|} arg.name pp arg.signature
    | None -> Pp.fp ppf "()"

  let pp_s pp_sig ppf args = Pp.fp ppf "%a"
      (Pp.(list ~sep:(s "→@,")) @@ pp pp_sig) args;
    if List.length args > 0 then Pp.fp ppf "→"
end

module Pkg = Paths.Pkg

module Origin = struct
  type source = Pkg.t

  type t =
    | Unit of source (** aka toplevel module *)
    | Extern (** aka unknown module *)
    | Alias of t (** M = A… *)
    | Submodule
    | First_class (** Not resolved first-class module *)
    | Arg (** functor argument *)
    | Rec (** mockup module for recursive definitions *)

  let rec pp ppf = function
    | Unit { Pkg.source= Local; _ } -> Pp.fp ppf "#"
    | Unit { Pkg.source = Pkg x; _ } -> Pp.fp ppf "#[%a]" Paths.Simple.pp x
    | Unit { Pkg.source = Unknown; _} -> Pp.fp ppf "#!"
    | Extern -> Pp.fp ppf "!"
    | Unit { Pkg.source = Special n; _} -> Pp.fp ppf "*(%s)" n
    | Rec -> Pp.fp ppf "?"
    | Submodule -> Pp.fp ppf "."
    | First_class -> Pp.fp ppf "'"
    | Arg -> Pp.fp ppf "§"
    | Alias n -> Pp.fp ppf "(≡%a)" pp n

    let rec reflect ppf = function
    | Unit pkg  -> Pp.fp ppf "Unit %a" Pkg.reflect pkg
    | Rec -> Pp.fp ppf "Rec"
    | Extern -> Pp.fp ppf "Unknown"
    | Submodule -> Pp.fp ppf "Submodule"
    | First_class -> Pp.fp ppf "First_class"
    | Arg -> Pp.fp ppf "Arg"
    | Alias n -> Pp.fp ppf {|Alias (%a)|} reflect n

  let at_most max v = match max, v with
    | (First_class|Rec|Arg|Extern| Alias _ ) , _ -> max
    | Unit _ , v -> v
    | Submodule, Unit _ -> Submodule
    | Submodule, Alias _ -> Submodule
    | Submodule, v -> v
end
type origin = Origin.t

type t = {
  name:Name.t;
  origin: Origin.t;
  args: t option list;
  signature:signature
}
and signature = { modules: mdict; module_types: mdict }
and mdict = t Name.Map.t

type arg = signature Arg.t
type modul = t

let of_arg ({name;signature}:arg) =
  { name; origin = Arg ; args=[]; signature }

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

let rec reflect ppf {name;args;origin;signature} =
  Pp.fp ppf {|@[<hov>{name="%s"; origin=%a; args=%a; signature=%a}@]|}
    name Origin.reflect origin reflect_args args reflect_signature signature
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


let rec pp ppf {name;args;origin;signature} =
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



let empty = Name.Map.empty

let create ?(args=[]) ?(origin=Origin.Submodule) name signature =
  { name; origin; args; signature}


module Sig = struct

  let card s =
    let card = Name.Map.cardinal in
    card s.modules + card s.module_types


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
end

module Partial = struct
  type nonrec t =
    { origin: Origin.t;
      args: t option list;
      result: signature }
  let empty = { origin = Submodule; args = []; result= Sig.empty }
  let simple defs = { empty with result = defs }

  let pp ppf (x:t) =
    if x.args = [] then
      Pp.fp ppf "%a(%a)" pp_signature x.result Origin.pp x.origin
    else Pp.fp ppf "%a@,→%a(%a)"
        pp_args x.args
        pp_signature x.result
        Origin.pp x.origin

  let no_arg x = { origin = Submodule; args = []; result = x }

  let drop_arg (p:t) = match  p.args with
    | _ :: args -> { p with args }
    | [] ->
      match p.origin with
      | Extern | First_class | Rec
      | Unit {Paths.P.source = Unknown; _ }
        -> p (* we guessed the arg wrong *)
      | Unit _ | Submodule | Arg | Alias _ ->
        Error.log "Only functor can be applied, got:%a" pp p

  let to_module ?origin name (p:t) =
    let origin = match origin with
      | Some o -> Origin.at_most p.origin o
      | None -> p.origin
    in
    {name;origin; args = p.args; signature = p.result }

  let to_arg name (p:t) =
    {name;origin=Arg; args = p.args; signature = p.result }

  let of_module {args;signature;origin; _} = {origin;result=signature;args}

  let is_functor x = x.args <> []

  let to_sign fdefs =
    if fdefs.args <> [] then
      ( Pp.fp Pp.err "%a@." pp fdefs;
        Error.signature_expected ()
      )
    else
      fdefs.result

end
