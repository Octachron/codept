type atomic = private Atomic
type many = private Many

type 'kind witness =
  | Atomic: atomic witness
  | Many: many witness

type 'kind t =
  | Atom: string -> atomic t
  | List: any list -> many t
and any = Any: 'any t -> any

let any sexp = Any sexp

let rec pp: type any. Format.formatter -> any t -> unit =
  fun ppf -> function
  | Atom s -> Pp.string ppf s
  | List l -> Pp.(list ~pre:(s"(") ~post:(s")") ~sep:(s " @,") @@ pp_any ) ppf l
and pp_any ppf (Any s) = pp ppf s

let parse_atom: type any. any t -> string option =
  function
  | List _ -> None
  | Atom s -> Some s

let extract_atom (Atom s) = s
let atom s = Atom s

let parse_list: type some. some t -> any list option = function
  | List l -> Some l
  | Atom _ -> None


let extract_list (List l) = l

let some x = Some x
let list l = List l

type ('a,'b) impl = {
  embed: 'a -> 'b t;
  parse: 'b t -> 'a option;
  witness: 'b witness
}

module U = struct

 type _ witness = ..

  module type key = sig
    val name: string
    type elt
    type _ witness += T: elt witness
    val default: elt
  end

  type 'a key = (module key with type elt = 'a )

  let key (type e) name default: e key =
    let module M = struct
      type elt = e
      let name = name
      let default = default
      type _ witness += T: elt witness
    end in
    (module M)

  let witness (type a) (key: a key) =
      let module K = (val key) in K.T

  let name (type a) (key: a key) =
    let module K = (val key) in K.name

  let default (type a) (key: a key) =
    let module K = (val key) in K.default

  type elt = E: { key:'a witness; value:'a } -> elt
  type mapper = M: { key:'a key; value: ('a, many) impl } -> mapper

  let extract (type a) (module M: key with type elt = a) (E elt): a =
    match elt.key with
    | M.T -> elt.value
    | _ -> M.default

    let extract_opt (type a) (module M: key with type elt = a) (E elt): a option =
    match elt.key with
    | M.T -> Some elt.value
    | _ -> None

  let extract_mapper (type a) (module M: key with type elt = a) (M m):
    (a, many) impl option =
    let module K = (val m.key) in
    match K.T with
    | M.T -> Some m.value
    | _ -> None

  module M = struct
    module I =Map.Make(struct type t = string let compare = compare end)
    type t = elt I.t
    type m = mapper I.t

    let empty:t = I.empty

    let find_opt key m =
      match I.find (name key) m with
      | exception Not_found -> None
      | x -> extract_opt key x

    let find key m =
      match I.find (name key) m with
      | exception Not_found -> default key
      | x -> extract key x

    let find_mapper key m =
      match I.find (name key) m with
      | exception Not_found -> None
      | x -> extract_mapper key x

    let add key value (m:t): t =
      I.add (name key) (E { key=witness key; value}) m

    let add_mapper key value m =
      I.add (name key) (M {key; value}) m

  end

end

let field key impl = U.M.add_mapper key impl
let definition =
  List.fold_left (|>) U.M.I.empty


let any_parse: type i. ('a,i) impl -> (any -> 'a option) =
  fun impl sexp ->
  match impl.witness, sexp with
  | Atomic, Any (Atom _ as sexp) -> impl.parse sexp
  | Many, Any (List _ as sexp) -> impl.parse sexp
  | _, _ -> None

open Option
let (%) f g x = f (g x)
let (%>) f g x = g (f x)

let atomic show parse =
  { parse = (fun s -> s |> extract_atom |> parse );
    embed = atom % show;
    witness = Atomic
  }

let list' impl =
  let parse sexp =
    sexp |> extract_list |> List.map (any_parse impl) |> list_join in
  { parse;
    embed = list % List.map (any % impl.embed);
    witness = Many;
  }

let map (impl_map: U.M.m) =
  let rec parse: many t -> U.M.t option = function
    | List [] -> Some U.M.empty
    | List ( Any (Atom _) :: _ ) -> None
    | List( Any (List l) :: q ) ->
      l |> parse_elt >>= fun (name,e) ->
      List q |> parse >>= fun map ->
      some @@ U.M.I.add name e map
  and parse_elt: any list -> (string * U.elt) option = function
    | [] -> None
    | Any (List _) :: _ -> None
    | Any (Atom a) :: q ->
      match U.M.I.find a impl_map with
      | exception Not_found -> None
      | U.M {key;value} ->
        value.parse (List q) >>| fun value ->
        (a, U.E { key = U.witness key; value})
  in
  let embed_elt name impl x =
    let (List l) = impl.embed x in
    List( Any (Atom name) :: l ) in
  let embed_key univ sexp (_name, U.M {key; value} ) =
    match U.M.find_opt key univ with
    | None -> sexp
    | Some x ->
      (any @@ embed_elt (U.name key) value x) :: sexp in
  let embed univ =
    list @@
    List.fold_left (embed_key univ) [] (U.M.I.bindings impl_map)
  in
  {parse;embed;witness = Many }

let record l = map @@ definition l

let keyed_list key impl =
  let parse = function
    | List [] -> None
    | List ( Any (Atom a) :: q) when a = key ->
      q |> List.map (any_parse impl) |> list_join
    | List (_ :: _) -> None
  in
  let embed s = list @@ Any (Atom key) :: ( List.map (any % impl.embed) s ) in
  {parse; embed; witness = Many}

type ('a,'b) bij = { f: 'a -> 'b; fr:'b -> 'a }

let id = { f = (fun x -> x); fr = (fun x -> x) }


let pair l r =
  let parse = function
    | List ([]| [_] |  _ :: _  :: _ :: _ ) -> None
    | List [a; b] -> any_parse l a && any_parse r b in
  let embed (a,b) = list [Any (l.embed a); Any (r.embed b)] in
  {parse;embed; witness = Many }

let pair' bij l r =
  let parse = function
    | List ([]| [_] |  _ :: _  :: _ :: _ ) -> None
    | List [a; b] -> (any_parse l a && any_parse r b) >>| bij.f in
  let embed s  =
    let a, b = bij.fr s in
    list [Any (l.embed a); Any (r.embed b)] in
  {parse;embed; witness = Many }


let opt: 'a 'b. ('a,'b) impl -> ('a option, many) impl =
  fun impl ->
  let parse = function
    | List [] -> Some None
    | List [a] -> any_parse impl a >>| some
    | List _ -> None
  in
  let embed = function
    | None -> List []
    | Some x -> List [ Any (impl.embed x) ] in
  {parse;embed; witness = Many }


let cons l r =
  let parse = function
    | List [] -> None
    | List (a :: b) ->
      (any_parse l a && r.parse (List b)) >>| fun (a, b) -> a :: b
  in
  let embed list  =
    match list with
    | []  -> raise (Invalid_argument "Not enough arg in Sexp.cons")
    | a :: b ->
    let a' = l.embed a in
    match r.embed b with
    | List l -> List( Any a' :: l ) in
  {parse;embed; witness = Many}

let empty =
  let parse = function
    | List [] -> Some []
    | List (_ :: _) -> None in
  let embed _ =
    List [] in
  {parse; embed; witness = Many}

let singleton impl =
  let parse = function
    | List [a] -> any_parse impl a >>| fun x -> [x]
    | _ -> None in
  let embed a =
    match a with
    | [] | _ :: _ :: _ ->
      raise (Invalid_argument "Sexp.singleton: wrong number of elements")
    | [a] ->
    List [Any (impl.embed a)] in
  {parse; embed; witness = Many}

let fix witness impl =
  let parse s = (impl()).parse s  in
  let embed o = (impl()).embed o in
  {witness;parse;embed}

let int0 s =
  try Some ( int_of_string s ) with
    Failure _ -> None

let int = atomic string_of_int int0
let string =
  let id x = x in
  atomic id some
