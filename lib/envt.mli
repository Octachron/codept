type context =
  | Signature of Module.signature
  | In_namespace of Module.dict

type module_provider = Name.t -> Module.t Outliner.query_result option

module Core :
sig
  type t = {
    top: Module.Dict.t;
    current: context;
    deps: Deps.t ref;
    providers: module_provider list;
  }
  include Outliner.envt_with_deps with type t := t
  val empty: t
  val start: Module.definition -> t
end

val libs: Name.t list -> module_provider
val open_world: unit -> module_provider

val start:
  ?open_approximation:bool -> Name.t list -> Namespaced.t list
  -> Module.Dict.t -> Core.t
