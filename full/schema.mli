open Scheme

type m val m: m name
type ml val ml:ml name
type mli val mli:mli name

type file val file:file name
type dependencies val dependencies: dependencies name

type local val local: local name
type lib val lib: lib name
type unknown val unknown: unknown name


type atlas val atlas: atlas name

type path = string list val path: path t
type dep = (path * (path * void)) tuple val dep: dep t
type dep_list = dep list val dep_list: dep_list t

type all_deps = (
  optional * local * dep_list * (
    optional * lib * (path * (path * (path * void))) tuple list * (
      optional * unknown * path list *
      void
    )
  )
) record
val all_deps: all_deps t


type item = (
  required * file * string * (
    required * dependencies * all_deps
    * void
  )
) record
val item: item t

type assoc = (
  required * m * path * (
    optional * ml * string * (
      optional * mli * string *
      void
    )
  )
) record
val assoc: assoc t

type deps = (
  required * atlas * assoc list * (
    required * dependencies * item list *
    void
  )
) record
val deps: deps t

val x: deps s
val schema: deps s
