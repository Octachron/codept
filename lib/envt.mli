
type context =
  | Signature of Module.signature
  | In_namespace of Module.dict

type negative_membership = Not_found | Negative
type module_provider = Name.t ->
  (Module.t Outliner.query_result, negative_membership) result

module Core :
sig
  type t = {
    top: Module.Dict.t;
    current: context;
    deps: Deps.Edge.t Paths.Pkg.Map.t ref;
    providers: module_provider list;
  }
  include Outliner.envt_with_deps with type t := t
  val empty: t
  val start: Module.definition -> t
end

val mask: Name.set -> module_provider
val libs: Name.t list -> module_provider
val open_world: module_provider

val start:
  ?open_approximation:bool -> Name.set -> Name.t list -> Module.Dict.t ->
  Core.t
