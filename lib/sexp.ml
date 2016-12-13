type atomic = private Atomic
type many = private Many

type 'kind kind =
  | Atomic: atomic kind
  | Many: many kind

type 'kind t =
  | Atom: string -> atomic t
  | List: any list -> many t
and any = Any: 'any t -> any

let rec any: type a. a t -> any = function
  | List [Any (Atom _ as sexp)] -> any sexp
  | sexp ->  Any sexp

let rec pp: type any. Format.formatter -> any t -> unit =
  fun ppf -> function
  | Atom s -> Pp.string ppf s
  | List l -> Pp.(list ~pre:(s"(") ~post:(s")") ~sep:(s " ") @@ pp_any ) ppf l
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
let list' l = List l

type ('a,'b) impl = {
  embed: 'a -> 'b t;
  parse: 'b t -> 'a option;
  witness: 'b kind
}

module U = struct

 type _ witness = ..

  module type key = sig
    val name: string
    type elt
    type atomicity
    type _ witness += T: (elt * atomicity) witness
    val kind: atomicity kind
    val default: elt
  end

  type ('a,'b) key = (module key with type elt = 'a and type atomicity = 'b )

  let key (type e k) kind name default: (e,k) key =
    let module M = struct
      type elt = e
      type atomicity = k
      let name = name
      let default = default
      let kind = kind
      type _ witness += T: (elt * k) witness
    end in
    (module M)

  let witness (type a k) (key: (a,k) key) =
      let module K = (val key) in K.T

  let name (type a k) (key: (a,k) key) =
    let module K = (val key) in K.name

  let default (type a k) (key: (a,k) key) =
    let module K = (val key) in K.default

  type elt = E: { key: ('a * 'b) witness; value:'a } -> elt
  type mapper = M: { key: ('a,'b) key; value: ('a, 'b) impl } -> mapper

  let extract (type a k) (key: (a,k) key ) (E elt): a =
    let module M = (val key) in
    match elt.key with
    | M.T -> elt.value
    | _ -> M.default

  let extract_opt (type a b) (key: (a,b) key) (E elt): a option =
    let module M = (val key) in
    match elt.key with
    | M.T -> Some elt.value
    | _ -> None

    let extract_mapper (type a b) (key: (a,b) key ) (M m): (a,b) impl option =
      let module M = (val key) in
      let module K = (val m.key) in
      match K.T with
      | M.T ->  Some m.value
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

module Record = struct
  let create = List.fold_left (|>) U.M.empty
  let field key value = U.M.add key value
  let get key r = U.M.find key r
end

let any_parse: type i. ('a,i) impl -> (any -> 'a option) =
  fun impl sexp ->
  match impl.witness, sexp with
  | Atomic, Any (Atom _ as sexp) -> impl.parse sexp
  | Many, Any (List _ as sexp) -> impl.parse sexp
  | Many, ( Any(Atom _ ) as sexp) -> impl.parse (List [sexp])
  | _, _ -> None

open Option
let (%) f g x = f (g x)
let (%>) f g x = g (f x)

let atomic show parse =
  { parse = (fun s -> s |> extract_atom |> parse );
    embed = atom % show;
    witness = Atomic
  }

let list impl =
  let parse sexp =
    sexp |> extract_list |> List.map (any_parse impl) |> list_join in
  { parse;
    embed = list' % List.map (any % impl.embed);
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
        let module K = (val key) in
        match K.kind, q with
        | Atomic, [Any(Atom s)] ->
          Atom s |> value.parse >>| fun value ->
          (a, U.E { key = U.witness key; value})
        | Many, q ->
          List q |> value.parse >>| fun value ->
          (a, U.E { key = U.witness key; value})
        | Atomic, ([] | _ ::_ :: _)  -> None
        | Atomic, [Any(List _) ] -> None in
  let embed_elt: type a k. string -> (a,k) impl -> a -> many t =
    fun name impl x ->
    match impl.witness with
    | Many ->
      let (List l) = impl.embed x in
      List( Any (Atom name) :: l )
    | Atomic ->
      List [ Any(Atom name); any (impl.embed x) ]
  in
  let embed_key univ sexp (_name, U.M {key; value} ) =
    match U.M.find_opt key univ with
    | None -> sexp
    | Some x ->
      if x <> U.default key then
        (any @@ embed_elt (U.name key) value x) :: sexp
      else
        sexp
  in
  let embed univ =
    list' @@
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
  let embed s = list' @@ Any (Atom key) :: ( List.map (any % impl.embed) s ) in
  {parse; embed; witness = Many}

type ('a,'b) bij = { f: 'a -> 'b; fr:'b -> 'a }

let id = { f = (fun x -> x); fr = (fun x -> x) }


type 'a constr = C: {
  name:Name.t;
  proj:'a -> 'b option;
  inj: 'b -> 'a;
  impl: ('b, 'kind) impl
} -> 'a constr

let sum l =
  let m = List.fold_left (fun acc (C cnstr) ->
      Name.Map.add cnstr.name (C cnstr) acc) Name.Map.empty l in
  let parse = function
    | List ([] | [_] |  _ :: _ :: _ :: _ ) -> None
    | List [ Any(List _); _ ] -> None
    | List [Any(Atom n) ; b] ->
      match Name.Map.find n m with
      | exception Not_found -> None
      | C cnstr -> (any_parse cnstr.impl b) >>| cnstr.inj
  in
  let embed x =
    let C cnstr = List.find (fun (C cnstr) -> cnstr.proj x <> None ) l in
    match cnstr.proj x with
    | None -> raise (Invalid_argument "Sexp.sum")
    | Some inner ->
      List[ Any (Atom cnstr.name); any (cnstr.impl.embed inner)] in
  {parse;embed;witness=Many}


let pair l r =
  let parse = function
    | List ([]| [_] |  _ :: _  :: _ :: _ ) -> None
    | List [a; b] -> any_parse l a && any_parse r b in
  let embed (a,b) = list' [any (l.embed a); any (r.embed b)] in
  {parse;embed; witness = Many }

let conv bij impl =
  let parse x =
    x |> impl.parse >>| bij.f in
  let embed s =
    s |> bij.fr |> impl.embed in
  {parse; embed; witness = impl.witness }

let convr impl f fr = conv {f;fr} impl

let opt: 'a 'b. ('a,'b) impl -> ('a option, many) impl =
  fun impl ->
  let parse = function
    | List [] -> Some None
    | List [a] -> any_parse impl a >>| some
    | List _ -> None
  in
  let embed = function
    | None -> List []
    | Some x -> List [ any (impl.embed x) ] in
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
    | List l -> List( any a' :: l ) in
  {parse;embed; witness = Many}

let pair' atom r =
  let parse = function
    | List [] -> None
    | List (a :: b) ->
      (any_parse atom a && r.parse (List b))
  in
  let embed (a,b)  =
    let a = atom.embed a in
    match r.embed b with
    | List l -> List( any a :: l ) in
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
    List [any (impl.embed a)] in
  {parse; embed; witness = Many}

let fix impl =
  let parse s = (impl()).parse s  in
  let embed o = (impl()).embed o in
  {witness = Many;parse;embed}

let int0 s =
  try Some ( int_of_string s ) with
    Failure _ -> None

let int = atomic string_of_int int0
let string =
  let id x = x in
  atomic id some

let unit = {
  parse = (function List [] -> Some () | _ -> None);
  embed = (fun () -> List []);
  witness = Many
}

let simple_constr name a =
  C { name; proj = (fun x -> if x = a then Some () else None)
    ; inj = (fun () -> a)
    ; impl = unit
    }
