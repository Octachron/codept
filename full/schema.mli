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
type dep = (path * (path * void)) Tuple.t val dep: dep t
type dep_list = dep list val dep_list: dep_list t

type all_deps = (
  local * dep_list option * (
    lib * (path * (path * (path * void))) Tuple.t list option * (
      unknown * path list option *
      void
    )
  )
) Record.t
val all_deps: all_deps t


type item = (
  file * string * (
    dependencies * all_deps * void
  )
) Record.t
val item: item t

type assoc = (
  m * path * (
    ml * string option * (
      mli * string option *
      void
    )
  )
) Record.t
val assoc: assoc t

type deps = (
  atlas * assoc list * (
    dependencies * item list *
    void
  )
) Record.t
val deps: deps t

val x: deps s
val schema: deps s
