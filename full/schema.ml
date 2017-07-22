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

type ml = Ml.t let ml = Ml.x
type mli = Mli.t let mli = Mli.x

type m = Module.t let m = Module.x

type file = File.t let file = File.x

type local = Local.t let local = Local.x
type unknown = Unknown.t let unknown = Unknown.x
type lib = Lib.t let lib = Lib.x

type dependencies = Dependencies.t
let dependencies = Dependencies.x

type atlas = Atlas.t
let atlas = Atlas.x


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
  title ="codept.0.10/deps";
  description = "dependencies and module mapping of ocaml project";
  sch = deps
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
