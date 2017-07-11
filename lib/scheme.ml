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
    | Array t -> Pp.fp ppf
                   "%a,@;@[<hov 2>%a@ :@ {@;%a@;}@]"
                   ty "array" k "items" json_type t
    | [] -> ()
    | _ :: _ as l ->
      Pp.fp ppf "%a,@; @[<hov 2>%a@ :@[%a]@]" ty "array" k "items"
        json_schema_tuple l
    | Obj r ->
      Pp.fp ppf "%a,@;@[<v 2>%a : {@;%a@;}@],@;@[<hov 2>%a@ :@ [@ %a@ ]@]"
        ty "object"
        k "properties"
        json_properties r
        k "required"
        (json_required true) r
and json_schema_tuple: type a. Format.formatter -> a tuple t -> unit =
  fun ppf -> function
    | [] -> ()
    | [a] -> Pp.fp ppf {|@[<hov 2>{@;%a@;}@]|}
               json_type a
    | a :: q ->
      Pp.fp ppf {|@[<hov 2>{@;%a@;}@],@; %a|}
        json_type a json_schema_tuple q
and json_properties: type a. Format.formatter -> a record_declaration -> unit =
  fun ppf -> function
  | [] -> ()
  | [_, n, a] -> Pp.fp ppf {|@[<hov 2>"%s" : {@;%a@;}@]|}
      (show n) json_type a
  | (_, n, a) :: q ->
     Pp.fp ppf {|@[<hov 2>"%s" : {@;%a@;},@;%a@]|}
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
    | String, s -> cstring ppf s
    | Array k, l ->
      Pp.fp ppf "@[<hov>(%a)@]"
        (Pp.list ~sep:(Pp.s "@ ") @@ sexp k) l
    | [], [] -> ()
    | [a], [x] -> sexp a ppf x
    | a :: q, x :: xs -> Pp.fp ppf "%a@ %a" (sexp a) x (sexp q) xs
    | Obj sch, x -> Pp.fp ppf "@[<hov>(@;<1 2>%a@;<1 2>)@]" (sexp_obj sch) x
and sexp_obj: type a.
  a record_declaration -> Format.formatter -> a record -> unit =
  fun sch ppf x -> match sch, x with
    | [], [] -> ()
    | (_, name,sch) :: q ,   (_, Just x) :: xs ->
      Pp.fp ppf {|(%a %a)|} cstring (show name) (sexp sch) x;
      begin match q, xs  with
        | [], [] -> ()
        | _ -> Pp.fp ppf "@ %a" (sexp_obj q) xs
      end
    | (Opt,_,_) :: q, (_, Nothing ) :: xs -> sexp_obj q ppf xs

let sexp x = sexp x.sch

let ($=) field x = field, Just x
let skip name = name, Nothing

let ($=?) field x = match x with
  | Some x -> field $= x
  | None -> skip field

let obj (x:_ record)= x
