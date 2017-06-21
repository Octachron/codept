
module Findmap = Map.Make(struct type t = Findlib.query let compare = compare end)

type t = {
  env: Envt.Core.t;
  signatures: Module.t list Name.map;
  m2l: Unit.s Name.map;
  findlib: ( (Common.task -> Common.task) * (unit -> unit) ) Findmap.t
}

let empty =
  { env = Envt.Core.empty;
    signatures = Name.Map.empty;
    m2l = Name.Map.empty;
    findlib = Findmap.empty
  }

module Shared = struct
  type 'a t = { lock:Mutex.t; mutable data:'a}
  type 'a shared = 'a t

let make data = { lock = Mutex.create ();data }

let get shared =
  Mutex.lock shared.lock;
  let data = shared.data in
  Mutex.unlock shared.lock;
  data

let set shared x =
  Mutex.lock shared.lock;
  shared.data <- x;
  Mutex.unlock shared.lock

  let map f shared =
    set shared @@ f @@ get shared
end
