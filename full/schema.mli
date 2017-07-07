open Scheme

type m val m: m name
type ml val ml:ml name
type mli val mli:mli name

type file val file:file name
type dependencies val dependencies: dependencies name
type atlas val atlas: atlas name

type path = string list val path: path t
type dep_list = path list val dep_list: dep_list t

type item = (
  required * file * string * (
    required * dependencies * dep_list
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
