open Scheme

module Module = Name(struct let s = "module" end)
module Ml = Name(struct let s = "ml" end)
module Mli = Name(struct let s = "mli" end)

module File = Name(struct let s = "file" end)
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

type dependencies = Dependencies.t
let dependencies: dependencies name = (module Dependencies)

type atlas = Atlas.t
let atlas: atlas name= (module Atlas)


let path = Array String
let dep_list = Array path
let item = Obj [
    Req, file, String;
    Req, dependencies, dep_list
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
type dep_list = path list

type item = (
  required * file * string * (
    required * dependencies * dep_list
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

