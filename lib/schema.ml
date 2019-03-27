open Schematic

module Lbl = struct
  module M2l = Label(struct let l = "m2l" end)
  module Deps = Label(struct let l = "deps" end)
  module Sig = Label(struct let l = "sig" end)
  type m2l = M2l.t
  type deps = Deps.t
  type sig' = Sig.t
end

let version = { Version.major = 0; minor=10; patch=0 }
let version2 = { Version.major = 0; minor=10; patch=3 }


let m2l = { Schematic.Ext.title = "codept/m2l/0.10";
            description = "module level ocaml file skeleton";
            version;
            label = Lbl.M2l.l;
            inner = M2l.sch
          }

let sign = { Schematic.Ext.title = "codept/sig/0.10";
             description = "module level ocaml signature";
             version;
             label = Lbl.Sig.l;
             inner = Array Module.Schema.module'
           }

module Module = Label(struct let l = "module" end)
module Ml = Label(struct let l = "ml" end)
module Mli = Label(struct let l = "mli" end)

let path = Array String
type p = Paths.S.t
type local_association =
  { path: p; ml: string option; mli: string option }

let raw_assoc =
  Obj [
    Req, Module.l, path <?> "Toplevel module";
    Opt, Ml.l, String <?> "Implementation (.ml) file";
    Opt, Mli.l, String <?> "Interface (.mli) file"
  ]
  <?> "This type keeps track of which implementation file (.ml) \
       and interface file (.mli) provided a toplevel module"

let local_association =
  custom ["deps"; "local" ] raw_assoc
    (fun r -> [Module.l $= r.path; Ml.l $=? r.ml; Mli.l $=? r.mli])
    Record.(fun [_, path; _,ml; _, mli ] -> { path; ml; mli } )

type library_module = { path:p; lib:p}
module Lib = Label(struct let l = "lib" end)

let lib =
  custom ["deps";"lib_association"]
    (Obj [Req, Module.l, path; Req, Lib.l, path] <?>
     "Library dependency: module path followed by the library path")
    (fun (r: library_module) -> [Module.l $= r.path; Lib.l $= r.lib])
    (fun [_,path;_,lib] -> {path;lib})


module Local = Label(struct let l = "local" end)
module Unknown = Label(struct let l ="unknown" end)
module Deps = Label(struct let l = "deps" end)


module File = Label(struct let l = "file" end)

type unit = { file: string; deps: p list }

let raw_unit =
  Obj [
    Req, File.l, String <?> "File name";
    Opt, Deps.l, Array path <?> "list of dependencies"
  ]

let unit =
  let e: _ list = [] in
  let ($=$) x l = if l = L.[] then x $=? None else x $=? Some l in
  custom ["deps"; "unit"; "deps"] raw_unit
    (fun d -> [ File.l $= d.file; Deps.l $=$ d.deps ])
    ( fun [_, file; _,deps] -> {file; deps = Option.default e deps } )



module Dependencies = Label(struct let l = "dependencies" end)
module Atlas = Label(struct let l = "atlas" end)

type deps = {
  dependencies: unit list;
  local: local_association list;
  library: library_module list;
  unknown: p list;
}

let deps = Obj [
    Req, Dependencies.l, Array unit <?> "Infered dependencies" ;
    Opt, Local.l, Array local_association <?> "Modules provided by local files";
    Opt, Lib.l, Array lib <?> "Modules provided by libraries";
    Opt, Unknown.l, Array path <?> "Unknown modules";
  ]

let deps =
  let list x = Option.default ([]: _ list) x in
  let ($=$) x l = if l = L.[] then x $=? None else x $=? Some l in
  let open Record in
  custom ["deps"; "main"] deps
    (fun {dependencies; local; library; unknown }->
       [ Dependencies.l $= dependencies;
         Local.l $=$ local;
         Lib.l $=$ library;
         Unknown.l $=$ unknown
       ] )
    (fun [_,dependencies;_,local;_,lib;_,u] ->
       {dependencies;
        local=list local;
        library=list lib;
        unknown=list u
       } )

let x  = {
  Ext.title ="codept.0.10/deps";
  description = "dependencies and module-to-files mapping of ocaml project";
  label = Lbl.Deps.l;
  version = version2;
  inner = deps;
}

let schema = x
