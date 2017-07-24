
type (_,_) eq = Eq : ('a,'a) eq
type void = (int,float) eq

module L = struct
  type 'a t = 'a list =
    | [  ]
    | (::) of 'a * 'a t
end

module Tuple = struct
  type 'a tuple =
    | []: void tuple
    | (::): 'a * 'b tuple -> ('a * 'b) tuple
  type 'a t = 'a tuple
end
open Tuple

module type name = sig type t val s:string end
type 'a name = (module name with type t = 'a)
module Name(X:sig val s:string end) =
struct
  type t
  type s = t
  include X
  module M = struct type t = s let s = s end
  let x: t name = (module M)
end

let show (type a) (module X:name with type t = a) = X.s

type required = private Required
type optional = private Optional

type (_,_,_) modal =
  | Opt:(optional,'a,'a option) modal
  | Req:(required,'a,'a) modal

(*
type ('m,'a) elt =
  | Just: 'a -> ('any,'a) elt
  | Nothing: (optional,'any) elt
*)


module Record = struct
  type 'a record =
    | []: void record
    | (::): ( 'a name * 'elt) * 'c record ->
      ('a * 'elt * 'c) record
  type 'a t = 'a record
end
open Record

type ('a,'b) bijection = { fwd:'a->'b;rev:'b -> 'a}

type 'hole t =
  | Float: float t
  | Int: int t
  | Bool: bool t
  | String: string t
  | Void: void t
  | Array: 'hole t -> 'hole list t
  | (::) : 'a t * 'b tuple t -> ('a * 'b) tuple t
  | []: void tuple t
  | Obj:'a record_declaration -> 'a record t
  | Custom: ('a,'b) custom -> 'a t
  | Sum: 'a sum_decl -> 'a sum t

and ('a,'b) custom = { fwd:'a -> 'b; rev:'b -> 'a; sch:'b t; recs:bool }
and 'a record_declaration =
  | []: void record_declaration
  | (::): ( ('m,'x,'fx) modal * 'a name * 'x t) * 'c record_declaration
    -> (  'a * 'fx * 'c ) record_declaration

and 'a sum_decl =
    | [] : void sum_decl
    | (::): 'a t * 'b sum_decl -> ('a * 'b) sum_decl

and (_,_) cons =
  | Z: 'a -> ('a * 'any,'a) cons
  | E: (void * 'any,'a) cons
  | S: ('a, 'n ) cons -> ('any * 'a, 'n) cons

and 'a sum = C: ('a, 'elt ) cons -> 'a sum

(*
type ('m,'a,'b) field = 'a name * ('m, 'b) elt*)

type 'a s = {
  title: string;
  description: string;
  sch: 'a t;
}

let k ppf name = Pp.fp ppf {|"%s"|} name
let p ppf (key,data)=
  Pp.fp ppf {|@[%a@ :@ "%s"@]|} k key data

let ty ppf data = p ppf ("type",data)

let rec json_type: type a. Format.formatter -> a t -> unit =
  fun ppf -> function
    | Float -> ty ppf "number"
    | Int -> ty ppf "number"
    | String -> ty ppf "string"
    | Bool -> ty ppf "string"
    | Void -> ()
    | Array t -> Pp.fp ppf
                   "%a,@;@[<hov 2>%a : {@ %a@ }@]"
                   ty "array" k "items" json_type t
    | [] -> ()
    | _ :: _ as l ->
      Pp.fp ppf "%a,@; @[<hov 2>%a :[@ %a@ ]@]" ty "array" k "items"
        json_schema_tuple l
    | Obj r ->
      Pp.fp ppf "%a,@;@[<v 2>%a : {@ %a@ }@],@;@[<hov 2>%a@ :@ [@ %a@ ]@]"
        ty "object"
        k "properties"
        json_properties r
        k "required"
        (json_required true) r
    | Custom { recs=true ; _ }-> Pp.fp ppf "@[<hov 2>%a : \"#\"@]" k "$ref"
    | Custom r -> json_type ppf r.sch
    | Sum decl ->
      Pp.fp ppf "@[<hov 2>%a :[%a]@]"
        k "anyOf" json_sum decl
and json_schema_tuple: type a. Format.formatter -> a tuple t -> unit =
  fun ppf -> function
    | [] -> ()
    | [a] -> Pp.fp ppf {|@[<hov 2>{@ %a@ }@]|}
               json_type a
    | a :: q ->
      Pp.fp ppf {|@[<hov 2>{@ %a@ }@],@; %a|}
        json_type a json_schema_tuple q
    | Custom _ -> assert false
and json_properties: type a. Format.formatter -> a record_declaration -> unit =
  fun ppf -> function
  | [] -> ()
  | [_, n, a] -> Pp.fp ppf {|@[<hov 2>"%s" : {@ %a@ }@]|}
      (show n) json_type a
  | (_, n, a) :: q ->
     Pp.fp ppf {|@[<hov 2>"%s" : {@ %a@ }@],@;%a|}
       (show n) json_type a json_properties q
and json_required: type a. bool ->Format.formatter -> a record_declaration
  -> unit =
  fun first ppf -> function
  | [] -> ()
  | (Req, n, _) :: q ->
    Pp.fp ppf {|%t"%s"%a|}
      (fun ppf -> if not first then Pp.fp ppf ",@ " else ())
      (show n)
      (json_required false) q
  | _ :: q -> json_required first ppf q
and json_sum: type a. Format.formatter -> a sum_decl -> unit =
  fun ppf -> function
  | [] -> ()
  | [a] -> Pp.fp ppf "{%a}@ " json_type a
  | a::q -> Pp.fp ppf "{%a},@,%a" json_type a json_sum q

let rec json: type a. a t -> Format.formatter -> a -> unit =
  fun sch ppf x -> match sch, x with
    | Int, n -> Pp.fp ppf "%d" n
    | Float, f -> Pp.fp ppf "%f" f
    | String, s -> Pp.estring ppf s
    | Bool, b -> Pp.fp ppf {|"%b"|} b
    | Void, _ -> .
    | Array k, l ->
      Pp.fp ppf "@[<hov>[%a]@]"
        (Pp.list ~sep:(Pp.s ",@ ") @@ json k) l
    | [], [] -> ()
    | _ :: _ as sch , l -> Pp.fp ppf "@[<hov>[%a]@]" (json_tuple sch) l
    | Obj sch, x -> Pp.fp ppf "@[<hv>{ %a }@]" (json_obj false sch) x
    | Custom c, x -> json c.sch ppf (c.fwd x)
    | Sum q, x -> json_sum 0 q ppf x
and json_sum: type a. int -> a sum_decl -> Format.formatter -> a sum -> unit =
  fun n sch ppf x -> match sch, x with
    | a :: _ , C Z x -> json [Int; a] ppf Tuple.[n; x]
    | _ , C E -> json Int ppf n
    | _ :: q, C S c -> json_sum (n+1) q ppf (C c)
    | [], _ -> .

and json_tuple: type a. a tuple t -> Format.formatter -> a tuple -> unit =
  fun sch ppf x -> match sch, x with
    | [], [] -> ()
    | [a], [x] -> json a ppf x
    | a :: q, x :: xs -> Pp.fp ppf "%a,@ %a" (json a) x (json_tuple q) xs
    | Custom _, _ -> assert false

and json_obj: type a.
  bool -> a record_declaration -> Format.formatter -> a record -> unit =
  fun not_first sch ppf x -> match sch, x with
    | [], [] -> ()
    | (Req, name,sch) :: q ,   (_, x) :: xs ->
      if not_first then Pp.fp ppf ",@ ";
      Pp.fp ppf {|@[<hov 2>"%s" :@ %a@]|} (show name) (json sch) x;
      Pp.fp ppf "%a" (json_obj true q) xs
    | (Opt,name,sch) :: q, (_,Some x) :: xs ->
      Pp.fp ppf {|@[<hov 2>"%s" :@ %a@]|} (show name) (json sch) x;
      Pp.fp ppf "%a" (json_obj true q) xs
    | (Opt,_,_) :: q, (_, None ) :: xs ->
      json_obj not_first q ppf xs

type tree = Leaf of int | N of tree * tree
let rec sch = Sum [Int; [tree;tree] ]
and tree: tree t = Custom {fwd;rev;sch; recs = true}
and rev: (int * ((tree * (tree * void)) Tuple.t * void)) sum -> tree = function
  | C(Z n) -> Leaf n
  | C(S (Z Tuple.[t1;t2])) -> N(t1,t2)
  | C(S (S _)) -> .
and fwd: tree -> _ sum = function
  | Leaf n -> C(Z n)
  | N (t1,t2) -> C(S (Z Tuple.[t1;t2]))

let f ppf = json tree ppf (Leaf 0)
let g ppf = json tree ppf (N(N (Leaf 0, Leaf 1), Leaf 2))



let json s = json s.sch
let json_schema ppf s =
  Pp.fp ppf
    "@[<v 2>{@ \
     %a,@;\
     %a,@;\
     %a,@;\
     %a\
      @ }@]@."
     p ("$schema", "http://json-schema.org/schema#")
     p ("title", s.title)
     p ("description", s.description)
     json_type s.sch

let cstring ppf s =
  begin try
      ignore(String.index s ' ');
      Pp.estring ppf s
    with
      Not_found -> Pp.string ppf s
  end

let rec sexp: type a. a t -> Format.formatter -> a -> unit =
  fun sch ppf x -> match sch, x with
    | Int, n -> Pp.fp ppf "%d" n
    | Float, f -> Pp.fp ppf "%f" f
    | Bool, b -> Pp.fp ppf "%b" b
    | String, s -> cstring ppf s
    | Void, _ -> .
    | Array k, l ->
      Pp.fp ppf "@[<hov>(%a)@]"
        (Pp.list ~sep:(Pp.s "@ ") @@ sexp k) l
    | Obj sch, x -> Pp.fp ppf "@[<hov>(@;<1 2>%a@;<1 2>)@]" (sexp_obj sch) x
    | [], [] -> ()
    | _ :: _ as tu, t -> Pp.fp ppf "@[<hov>(@;%a@;)@]" (sexp_tuple tu) t
    | Custom r, x -> sexp r.sch ppf (r.fwd x)
    | Sum s, x -> sexp_sum 0 s ppf x
and sexp_tuple: type a. a Tuple.t t -> Format.formatter -> a Tuple.t -> unit =
  fun ty ppf t -> match ty, t with
    | [], [] -> ()
    | [a], [x] -> sexp a ppf x
    | a :: q, x :: xs -> Pp.fp ppf "%a@ %a" (sexp a) x (sexp_tuple q) xs
    | Custom _, _ -> assert false
and sexp_sum: type a. int -> a sum_decl -> Format.formatter -> a sum -> unit =
  fun n decl ppf x -> match decl, x with
    | a :: _ , C Z x -> sexp [Int;a] ppf Tuple.[n;x]
    | _, C E -> sexp Int ppf n
    | _ :: q, C S c -> sexp_sum (n+1) q ppf (C c)
    | [] , _ -> .
and sexp_obj: type a.
  a record_declaration -> Format.formatter -> a record -> unit =
  fun sch ppf x -> match sch, x with
    | [], [] -> ()
    | (Req, name,sch) :: q ,   (_, x) :: xs ->
      Pp.fp ppf {|(%a %a)|} cstring (show name) (sexp sch) x;
      begin match q, xs  with
        | [], [] -> ()
        | _ -> Pp.fp ppf "@ %a" (sexp_obj q) xs
      end
    | (Opt, name,sch) :: q ,   (_, Some x) :: xs ->
      Pp.fp ppf {|(%a %a)|} cstring (show name) (sexp sch) x;
      begin match q, xs  with
        | [], [] -> ()
        | _ -> Pp.fp ppf "@ %a" (sexp_obj q) xs
      end

    | (Opt,_,_) :: q, (_, None ) :: xs -> sexp_obj q ppf xs

let sexp x = sexp x.sch

let ($=) field x = field, x
let skip name = name, None

let ($=?) field x = match x with
  | Some x -> field $= Some x
  | None -> skip field

let obj (x:_ record)= x
let custom ?(recs=false) sch fwd rev = Custom {fwd;rev; sch; recs}

module Untyped = struct
type t =
  | Array of t list
  | Array_or_obj of t list
  | Atom of string
  | Obj of (string * t) list
type untyped = t
end
type untyped = Untyped.t =
  | Array of untyped list
  | Array_or_obj of untyped list
  | Atom of string
  | Obj of (string * untyped) list

let promote_to_obj l =
  let promote_pair = function Array [Atom x;y] -> Some(x,y) | _ -> None in
  Option.List'.map promote_pair l

let rec retype: type a. a t -> untyped -> a option =
  let open Option in
  fun sch u -> match sch, u with
    | Int, Atom u -> int_of_string_opt u
    | Float, Atom u -> float_of_string_opt u
    | String, Atom s -> Some s
    | Array t, (Array ul | Array_or_obj ul) ->
      Option.List'.map (retype t) ul
    |  [], Array [] -> Some []
    | (a::q), Array(ua :: uq) ->
        retype a ua >>= fun h ->
        retype q (Array uq) >>| fun q ->
        Tuple.(h :: q)
    | Obj r, Obj ur ->
      retype_obj r ur
    | Obj r, Array_or_obj ur ->
      promote_to_obj ur >>= fun obj ->
      retype_obj r obj
    | Custom r, x -> retype r.sch x >>| r.rev
    | Sum s, Atom x ->
      int_of_string_opt x >>= fun n ->
      retype_const_sum s n
    | Sum s, Array[Atom x; y] ->
      int_of_string_opt x >>= fun n ->
      retype_sum s n y
    | _ -> None
and retype_obj: type a. a record_declaration -> (string * untyped) list ->
  a record option = fun sch x ->
  let open Option in
  match sch, x with
  | [], [] -> Some []
  | (Req, field, t) :: q , (ufield, u) :: uq when show field = ufield ->
    retype t u >>= fun h ->
    retype_obj q uq >>| fun l ->
    Record.( (field $= h) :: l )
  | (Opt, field, t) :: q , (ufield, u) :: uq when show field = ufield ->
    retype t u >>= fun h ->
    retype_obj q uq >>| fun l ->
    Record.( (field $=? Some h) :: l )
  | (Opt,field,_) :: q , l ->
    retype_obj q l >>| fun l ->
    Record.( skip field :: l )
  | _ -> None
and retype_sum: type a. a sum_decl -> int -> untyped -> a sum option =
  let open Option in
  fun decl n u -> match decl, n with
    | (a :: _) , 0 ->
      retype a u >>| fun a -> C(Z a)
    | [], _n -> None
    | ( _:: q), n ->
      retype_sum q (n-1) u >>| fun (C c) -> (C (S c))

and retype_const_sum: type a. a sum_decl -> int -> a sum option =
  let open Option in
  fun decl n -> match decl, n with
    | (Void :: _ ), 0 -> Some (C E)
    | [], _n -> None
    | _ :: q, n ->
      retype_const_sum q (n-1) >>| fun (C c) -> (C (S c))
