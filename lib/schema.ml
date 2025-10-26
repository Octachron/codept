open Schematic

module Lbl = struct
  module M2l = Label(struct let l = "m2l" end)
  module Deps = Label(struct let l = "deps" end)
  module Namespace = Label(struct let l = "namespace" end)
  type m2l = M2l.t
  type deps = Deps.t
  type namespace = Namespace.t
end

let version = { Version.major = 0; minor=11; patch=0 }


let m2l = { Schematic.Ext.title = "codept/m2l/0.11.0";
            description = "module level ocaml file skeleton";
            version;
            label = Lbl.M2l.l;
            inner = M2l.sch
          }

let namespace = { Schematic.Ext.title = "codept/namespace/0.11.0";
             description = "namespaced compilation units";
             version;
             label = Lbl.Namespace.l;
             inner = Module.Namespace.sch
           }



module Module = Label(struct let l = "module" end)
module Ml = Label(struct let l = "ml" end)
module Mli = Label(struct let l = "mli" end)

let path: _ s = Namespaced.sch
type p = Namespaced.t

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
  custom raw_assoc
    (fun r -> [Module.l $= r.path
              ; Ml.l $=? r.ml
              ; Mli.l $=? r.mli])
    Record.(fun [_,path; _,ml; _, mli ] ->
        { path; ml; mli } )

type library_module = { path:p; lib:p}
module Lib = Label(struct let l = "lib" end)

module R = Schematic.Record
let lib =
  custom
    (Obj [Req, Module.l, path; Req, Lib.l, path] <?>
     "Library dependency: module path followed by the library path")
    (fun (r: library_module) -> [Module.l $= r.path; Lib.l $= r.lib])
    (let open R in fun [_,path;_,lib] -> {path;lib})


module Local = Label(struct let l = "local" end)
module Unknown = Label(struct let l ="unknown" end)
module Deps = Label(struct let l = "deps" end)


module File = Label(struct let l = "file" end)

module Externals = Label(struct let l = "externals" end)

type unit = { file: string; deps: p list; externals: string list }

let raw_unit =
  Obj [
    Req, File.l, String <?> "File name";
    Opt, Deps.l, Array path <?> "list of dependencies";
    Opt, Externals.l, Array String <?> "list of external primitive dependencies";
  ]

let unit =
  let e x = Option.default ([] : _ list) x in
  let ($=$) x l = if l = L.[] then x $=? None else x $=? Some l in
  custom  raw_unit
    (fun d -> [ File.l $= d.file; Deps.l $=$ d.deps; Externals.l $=$ d.externals ])
    (let open R in fun [_, file; _,deps; _, externals ] ->
        {file; deps = e deps; externals = e externals }
    )



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
  custom  deps
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
        unknown=list u;
       } )


let x  = {
  Ext.title ="codept.0.11.0/deps";
  description = "dependencies and module-to-files mapping of ocaml project";
  label = Lbl.Deps.l;
  version;
  inner = deps;
}

let schema = x
