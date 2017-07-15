open Scheme

module Module = Name(struct let s = "module" end)
module Ml = Name(struct let s = "ml" end)
module Mli = Name(struct let s = "mli" end)

module File = Name(struct let s = "file" end)

module Local = Name(struct let s = "local" end)
module Lib = Name(struct let s = "lib" end)
module Unknown = Name(struct let s ="unknown" end)

module Dependencies = Name(struct let s = "dependencies" end)
module Atlas = Name(struct let s = "atlas" end)

type ml = Ml.t
let ml: ml name = (module Ml)

type mli = Mli.t
let mli: mli name = (module Mli)

type m = Module.t
let m: m name = (module Module)

type file = File.t
let file: file name= (module File)

type local = Local.t let local: local name = (module Local)
type unknown = Unknown.t let unknown: unknown name = (module Unknown)
type lib = Lib.t let lib: lib name = (module Lib)

type dependencies = Dependencies.t
let dependencies: dependencies name = (module Dependencies)

type atlas = Atlas.t
let atlas: atlas name= (module Atlas)


let path = Array String
let dep: _ tuple t = [path; path]
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
  title ="codept.0.10/deps";
  description = "dependencies and module mapping of ocaml project";
  sch = deps
}

let schema = x

type path = string list
type dep = (path * (path * void)) tuple
type dep_list = dep list

type all_deps = (
  optional * local * dep_list * (
    optional * lib * (path * (path * (path * void))) tuple list * (
      optional * unknown * path list *
      void
    )
  )
) record

type item = (
  required * file * string * (
    required * dependencies * all_deps
    * void
  )
) record

type assoc = (
  required * m * path * (
    optional * ml * string * (
      optional * mli * string *
      void
    )
  )
) record

type deps = (
  required * atlas * assoc list * (
    required * dependencies * item list *
    void
  )
) record
