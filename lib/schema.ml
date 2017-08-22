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

type unit_association =
  { module_path: Name.t list ; ml: string option; mli: string option }

module Module = Label(struct let l = "module" end)
module Ml = Label(struct let l = "ml" end)
module Mli = Label(struct let l = "mli" end)

let path = Array String
let raw_assoc =
  Obj [
    Req, Module.l, path <?> "Toplevel module";
    Opt, Ml.l, String <?> "Implementation (.ml) file";
    Opt, Mli.l, String <?> "Interface (.mli) file"
  ]
  <?> "This type keeps track of which implementation file (.ml) \
       and interface file (.mli) provided a toplevel module"


let unit_association =
  custom ["deps";"assoc"] raw_assoc
    (fun r -> [Module.l $= r.module_path; Ml.l $=? r.ml; Mli.l $=? r.mli])
    Record.(fun [_, module_path; _,ml; _, mli ] -> { module_path; ml; mli } )


module Local = Label(struct let l = "local" end)
module Lib = Label(struct let l = "lib" end)
module Unknown = Label(struct let l ="unknown" end)

type p = Paths.S.t
type local = { path: p; file:p}
type lib = { path: p; lib: p; file:p}
type unknown = p
type dep = { local: local list; lib: lib list; unknown: unknown list }

let local =
  let open Tuple in
  custom ["deps";"unit";"dep";"local"]
    ([path;path] <?>
     "Local dependency: toplevel module followed by the associated file path" )
    (fun (r: local) -> [r.path;r.file])
    (fun [path;file] -> {path;file})


let lib =

  let open Tuple in
  custom ["deps";"unit";"dep";"lib"]
    ([path;path;path] <?>
     "Library dependency: module path followed by the library file path \
      and the relative file path of the specific module" )
    (fun (r: lib) -> [r.path;r.lib;r.file])
    (fun [path;lib;file] -> {path;lib;file})

let raw_dep =
  Obj [
    Opt, Local.l, Array local ;
    Opt, Lib.l, Array lib;
    Opt, Unknown.l,
    Array path <?> "List of unknown toplevel modules appearing in the input files"
  ]
  <?> "Dependencies for a unit file are divided in three groups: \
       local dependencies, library dependencies, and unknown dependencies."

let unit_deps =
  let open Record in
  let ($=) x l = if l = L.[] then x $=? None else x $=? Some l in
  custom ["deps"; "unit"; "deps"] raw_dep
    (fun d ->
       [Local.l $= d.local; Lib.l $= d.lib; Unknown.l $= d.unknown])
    Record.( fun [_,local;_,lib;_,unknown] ->
        let local, lib, unknown = Option.( local >< [], lib >< [], unknown >< [] ) in
        {local;lib;unknown} )

module File = Label(struct let l = "file" end)


module Dependencies = Label(struct let l = "dependencies" end)
module Atlas = Label(struct let l = "atlas" end)

type item = { file: string; dependencies: dep }
let raw_item =
  Obj [
    Req, File.l, String <?> "File name";
    Req, Dependencies.l, unit_deps
  ]
  <?> "Dependencies for a file"


let item = let open Record in
  custom ["deps"; "unit"; "item"] raw_item
    (fun {file;dependencies} -> [File.l $= file; Dependencies.l $= dependencies] )
    (fun [_,file;_,dependencies] -> {file;dependencies} )

type deps = { dependencies: item list; atlas: unit_association list }

let deps = Obj [
    Req, Dependencies.l, Array item <?> "Infered dependencies" ;
    Req, Atlas.l,
    Array unit_association <?> "Mapping between toplevel modules and files";
  ]

let deps = let open Record in
  custom ["deps"; "main"] deps
    (fun {dependencies; atlas}->
       [ Dependencies.l $= dependencies; Atlas.l $= atlas] )
    (fun [_,dependencies;_,atlas] -> {dependencies;atlas} )

let x  = {
  Ext.title ="codept.0.10/deps";
  description = "dependencies and module-to-files mapping of ocaml project";
  label = Lbl.Deps.l;
  version;
  inner = deps;
}

let schema = x
