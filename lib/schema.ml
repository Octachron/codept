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

module Module = Label(struct let l = "module" end)
module Ml = Label(struct let l = "ml" end)
module Mli = Label(struct let l = "mli" end)

module File = Label(struct let l = "file" end)

module Local = Label(struct let l = "local" end)
module Lib = Label(struct let l = "lib" end)
module Unknown = Label(struct let l ="unknown" end)

module Dependencies = Label(struct let l = "dependencies" end)
module Atlas = Label(struct let l = "atlas" end)

type ml = Ml.t let ml = Ml.l
type mli = Mli.t let mli = Mli.l

type m = Module.t let m = Module.l

type file = File.t let file = File.l

type local = Local.t let local = Local.l
type unknown = Unknown.t let unknown = Unknown.l
type lib = Lib.t let lib = Lib.l

type dependencies = Dependencies.t
let dependencies = Dependencies.l

type atlas = Atlas.t
let atlas = Atlas.l


let path = Array String
let dep: _ Tuple.t t = [path; path]
let dep_list = Array dep

let all_deps = Obj [
    Opt, local, dep_list;
    Opt, lib, Array [path; path; path];
    Opt, unknown, Array path
  ]
let item = Obj [
    Req, file, String;
    Req, dependencies, all_deps
  ]
let assoc =
  Obj [
    Req, m, path;
    Opt, ml, String;
    Opt, mli, String
  ]

let deps = Obj [
    Req, atlas, Array assoc;
    Req, dependencies, Array item
  ]

let x  = {
  Ext.title ="codept.0.10/deps";
  description = "dependencies and module mapping of ocaml project";
  label = Lbl.Deps.l;
  version;
  inner = deps;
}

let schema = x

type path = string list
type dep = (path * (path * void)) Tuple.t
type dep_list = dep list


type all_deps = (
  local * dep_list option * (
    lib * (path * (path * (path * void))) Tuple.t list option * (
      unknown * path list option *
      void
    )
  )
) Record.t

type item = (
  file * string * (
    dependencies * all_deps * void
  )
) Record.t

type assoc = (
  m * path * (
    ml * string option * (
      mli * string option *
      void
    )
  )
) Record.t

type deps = (
  atlas * assoc list * (
    dependencies * item list *
    void
  )
) Record.t
