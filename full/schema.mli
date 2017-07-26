open Schematic

type m val m: m label
type ml val ml:ml label
type mli val mli:mli label

type file val file:file label
type dependencies val dependencies: dependencies label

type local val local: local label
type lib val lib: lib label
type unknown val unknown: unknown label


type atlas val atlas: atlas label

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
