type void = private Void

module L = struct
  type 'a t = 'a list =
    | [  ]
    | (::) of 'a * 'a t
end

type 'a tuple =
  | []: void tuple
  | (::): 'a * 'b tuple -> ('a * 'b) tuple

module type name = sig type t val s:string end
module Name(X:sig val s:string end) = struct type t include X end

type 'a name = (module name with type t = 'a)
let show (type a) (module X:name with type t = a) = X.s

type required = private Required
type optional = private Optional
type _ modal =
  | Opt:optional modal
  | Req:required modal

type ('m,'a) elt =
  | Just: 'a -> ('any,'a) elt
  | Nothing: (optional,'any) elt

type 'hole t =
  | Float: float t
  | Int: int t
  | String: string t
  | Array: 'hole t -> 'hole list t
  | (::) : 'a t * 'b tuple t -> ('a * 'b) tuple t
  | []: void tuple t
  | Obj:'a record_declaration -> 'a record t

and 'a record_declaration =
  | []: void record_declaration
  | (::): ( 'm modal * 'a name * 'b t) * 'c record_declaration
    -> (  'm * 'a * 'b * 'c ) record_declaration

and 'a record =
  | []: void record
  | (::): ( 'a name * ('m,'b) elt) * 'c record ->
    ('m * 'a * 'b * 'c) record

type ('m,'a,'b) field = 'a name * ('m, 'b) elt


let rec json: type a. a t -> Format.formatter -> a -> unit =
  fun sch ppf x -> match sch, x with
    | Int, n -> Pp.fp ppf "%d" n
    | Float, f -> Pp.fp ppf "%f" f
    | String, s -> Pp.estring ppf s
    | Array k, l ->
      Pp.fp ppf "@[<hov>[%a]@]"
        (Pp.list ~sep:(Pp.s ",@ ") @@ json k) l
    | [], [] -> ()
    | [a], [x] -> json a ppf x
    | a :: q, x :: xs -> Pp.fp ppf "%a,@ %a" (json a) x (json q) xs
    | Obj sch, x -> Pp.fp ppf "@[<hov>{@;<1 2>%a@;<1 2>}@]" (json_obj sch) x
and json_obj: type a.
  a record_declaration -> Format.formatter -> a record -> unit =
  fun sch ppf x -> match sch, x with
    | [], [] -> ()
    | (_, name,sch) :: q ,   (_, Just x) :: xs ->
      Pp.fp ppf {|"%s":%a|} (show name) (json sch) x;
      begin match q, xs  with
        | [], [] -> ()
        | _ -> Pp.fp ppf ",@ %a" (json_obj q) xs
      end
    | (Opt,_,_) :: q, (_, Nothing ) :: xs ->
      json_obj q ppf xs

let ($=) field x = field, Just x
let skip name = name, Nothing

let ($=?) field x = match x with
  | Some x -> field $= x
  | None -> skip field

let obj (x:_ record)= x
