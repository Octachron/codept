module O = Zipper.Dep_fold.Default

let () =
  List.iter Format_tags.enable Format.[std_formatter;err_formatter]


module LocalSet =
  Set.Make(struct type t = Schema.local_association let compare=compare end)
module LibSet =
  Set.Make(struct type t = Schema.library_module let compare=compare end)


let build_atlas (lib,unknow) deps  =
  let build_atlas {Deps.path; pkg; _} (lib,unknw)  =
    let add_libs lib path = LibSet.add { Schema.lib; path } in
    match pkg.source with
    | Paths.P.Local|Paths.P.Special _ -> lib, unknw
    | Paths.P.Pkg pkg' -> add_libs pkg' path lib, unknw
    | Paths.P.Unknown -> lib, Paths.S.Set.add path unknw in
  Deps.fold build_atlas deps (lib,unknow)

let pp file name deps =
  let lib, unknown = build_atlas (LibSet.empty, Paths.S.Set.empty) deps in
  let pp = Schematic.Ext.json Schema.x in
  let dependencies = [{ Schema.file; deps= Deps.paths deps }] in
  let local = [{Schema.path=[name]; ml=Some file; mli=None}] in
  let library = LibSet.elements lib in
  let unknown = Paths.S.Set.elements unknown in
  Format.printf "%a@." pp {dependencies;local; library; unknown}

let () =
  let file = Sys.argv.(1) in
  let name, res =
    Read.file {Read.format=Read.Src; kind=M2l.Structure} file in
  let m2l =
    match res with
    | Error _ -> Format.eprintf "Error at parsing.@."; exit 2
    | Ok x -> x in
  let param = Transforms.{
      policy = Standard_policies.default;
      epsilon_dependencies = false;
      transparent_extension_nodes = false;
      transparent_aliases = true
    } in
  let env =   Envt.start ~open_approximation:true
    ~libs:[]
    ~namespace:[]
    ~implicits:[["Stdlib"], Bundle.stdlib ]
    Module.Dict.empty in
  let res = O.next param env (O.initial m2l) ~pkg:(Paths.Pkg.local file) in
  match res with
  | Error _ -> Format.printf "Error: unfinished outlining.@."; exit 2
  | Ok (_sig, deps) ->
    pp file name deps
