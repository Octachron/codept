type context =
  | Signature of Module.signature
  | In_namespace of Module.dict

type module_provider = Uloc.t -> Name.t -> Module.t Transforms.query_result option

module Core :
sig
  type t
  include Stage.envt with type t := t
  val empty: t
  val start: Module.definition -> t
end

val libs: Name.t list -> module_provider
val open_world: unit -> module_provider

val start:
  ?open_approximation:bool
  -> libs:Paths.S.t
  -> namespace: Namespaced.t list
  -> implicits:(Paths.S.t * Module.Dict.t) list
  -> Module.Dict.t -> Core.t
