let () = Random.self_init ()
module Failing_env = struct
  include Envt.Core
  let fail = ref true
  let find loc ?edge level path env =
    if !fail then (
      fail := false;
      raise Not_found
    )
    else
      ( fail := true;
        find loc ?edge level path env
      )
end

module Param = struct
let policy = Standard_policies.default
let epsilon_dependencies = false
let transparent_extension_nodes = false
let transparent_aliases = true
end

module O = Dep_zipper.Make(Failing_env)(Param)

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
    | Pkg.Local|Pkg.Special _ -> lib, unknw
    | Pkg.Pkg pkg' -> add_libs pkg' path lib, unknw
    | Pkg.Unknown -> lib, Namespaced.Set.add path unknw in
  Deps.fold build_atlas deps (lib,unknow)

let pp file name deps =
  let lib, unknown = build_atlas (LibSet.empty, Namespaced.Set.empty) deps in
  let pp = Schematic.Ext.json Schema.x in
  let dependencies = [{ Schema.file; deps= Deps.paths deps }] in
  let local = [{Schema.path=Namespaced.make name; ml=Some file; mli=None}] in
  let library = LibSet.elements lib in
  let unknown = Namespaced.Set.elements unknown in
  Format.printf "%a@." pp {dependencies;local; library; unknown}

let () =
  let file = Sys.argv.(1) in
  let name, res =
    Read.file {Read.format=Read.Src; kind=M2l.Structure} file in
  let m2l =
    match res with
    | Error _ -> Format.eprintf "Error at parsing.@."; exit 2
    | Ok x -> x in
  let env =   Envt.start ~open_approximation:true
    ~libs:[]
    ~namespace:[]
    ~implicits:[["Stdlib"], Bundle.stdlib ]
    Module.Dict.empty in
  let pkg = (Pkg.local file) in
  let rec loop guard res = match O.next env res ~pkg with
    | Error zipper ->
      if guard = 0 then
        (Format.printf "Error: unfinished outlining.@."; exit 2)
      else
        loop (guard-1) zipper
    | Ok (_sig, deps) -> deps in
  let deps = loop 1_000_000 (O.initial m2l) in
  pp file (Modname.to_string name) deps
