
type format = Json | Sexp

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


module type label = sig type t val l:string end
type 'a label = (module label with type t = 'a)
module Label(X:sig val l:string end) =
struct
  type t
  type s = t
  include X
  module M = struct type t = s let l = l end
  let l: t label = (module M)
end

let show (type a) ((module X):a label) = X.l

type required = private Required
type optional = private Optional

type (_,_,_) modal =
  | Opt:(optional,'a,'a option) modal
  | Req:(required,'a,'a) modal


module Record = struct
  type 'a record =
    | []: void record
    | (::): ( 'a label * 'elt) * 'c record ->
      ('a * 'elt * 'c) record
  type 'a t = 'a record
end
open Record

type ('a,'b) bijection = { fwd:'a->'b;rev:'b -> 'a}

type 'a r = Indexed

type ('hole, 'free) s =
  | Float: (float, 'free) s
  | Int: (int, 'free) s
  | Bool: (bool, 'free) s
  | String: (string, 'free) s
  | Void: (void, 'free) s
  | Array: ('hole,'free) s -> ('hole list, 'free) s
  | (::) : ('a,'free) s * ('b tuple, 'free) s -> (('a * 'b) tuple, 'free) s
  | []: (void tuple, 'free) s
  | Obj: ('a,'free) record_declaration -> ('a record, 'free) s
  | Custom: ('a,'b,'free) custom -> ('a, 'free) s
  | Sum: ('a,'free) sum_decl -> ('a sum,'free) s
  | Description: string * ('hole,'free) s -> ('hole, 'free) s
  | Rec: { id: string list; defs:('defs,'defs r) rec_defs; proj: ('defs, 'res) index}
      -> ('res,'free) s
  | Var: ('free, 'result) index -> ('result,'free r) s

and (_,_) rec_defs =
  | []: (void,'free r) rec_defs
  | (::): (string * ('a,'free r) s) * ('l, 'free r) rec_defs -> ('a * 'l, 'free r) rec_defs

and ('a,'b,'free) custom = { fwd:'a -> 'b; rev:'b -> 'a; sch:('b,'free) s }
and ('a,'free) record_declaration =
  | []: (void, 'free) record_declaration
  | (::): ( ('m,'x,'fx) modal * 'a label * ('x,'free) s) * ('c,'free) record_declaration
    -> (  'a * 'fx * 'c, 'free) record_declaration

and ('a,'mu) sum_decl =
    | [] : (<before:void>, 'mu) sum_decl
    | (::): (string * ('a,'mu) s) * ('b,'mu) sum_decl
        -> (<at:'a; before:'b>,'mu) sum_decl

and (_,_) cons =
  | Z: 'a -> (<at:'a; before: 'any>,'a) cons
  | E: (<at:void; before:'any>,'a) cons
  | S: ('a, 'n ) cons -> (<at:'any; before:'a>, 'n) cons

and 'a sum = C: ('a, 'elt ) cons -> 'a sum

and (_,_) index =
  | Zn: ( 'a * 'b  , 'a) index
  | Sn: ('list, 'res) index -> ( _ * 'list, 'res) index

type 'a t = ('a,void) s
type 'a schematic = 'a t


let rec get: type a all res. (a,all r) rec_defs -> (a, res) index -> (res,all r) s = fun l x ->
  match l, x with
  | (_,a)::_ , Zn -> a
  | _ :: q, Sn x -> get q x
  | [], _ -> .


let rec reopen: type a b. a t -> (a,b) s =
  function
  | Float -> Float
  | Int -> Int
  | Void -> Void
  | Bool -> Bool
  | String -> String
  | Rec {id;proj;defs} -> Rec {id;proj;defs}
  | [] -> []
  | x :: q -> reopen x :: reopen q
  | Array x -> Array (reopen x)
  | Obj q -> Obj (reopen_obj q)
  | Custom {rev;fwd;sch} -> Custom {rev;fwd;sch=reopen sch}
  | Sum x -> Sum (reopen_sum x)
  | Description(d,x) -> Description(d, reopen x)
  |   _ -> .
and reopen_obj: type a f. (a,void) record_declaration -> (a,f) record_declaration = function
  | [] -> []
  | (Req, lbl, a) :: q -> (Req, lbl, reopen a) :: reopen_obj q
  | (Opt, lbl, a) :: q -> (Opt, lbl, reopen a) :: reopen_obj q
and reopen_sum: type a f. (a,void) sum_decl -> (a,f) sum_decl = function
    | (name, a) :: q -> (name, reopen a) :: reopen_sum q
    | [] -> []

let custom sch fwd rev = Custom {fwd;rev; sch}
module Version = struct
  type t = { major:int; minor:int; patch:int }
  module Lbl = Label(struct let l = "version" end)
  type lbl = Lbl.t

  let sch =
  custom [Int;Int;Int]
    (fun x -> [x.major;x.minor;x.patch])
    (fun [major;minor;patch] -> {major;minor;patch})

end

let ($=) field x = field, x

let k ppf name = Pp.fp ppf {|"%s"|} name
let p ppf (key,data)=
  Pp.fp ppf {|@[%a@ :@ "%s"@]|} k key data

let ty ppf data = p ppf ("type",data)


type 'a tree =
  | Item of string list * 'a
  | M of 'a forest
and 'a forest = 'a tree Name.map

let arbitrary_name ctx = "id" ^ string_of_int ctx, ctx + 1

let find_nearly name map =
 let find k arg name =
    match Name.Map.find name map with
    | exception Not_found -> name, Name.Map.empty
    | Item _ -> k arg
    | M m -> name, m in
 let rec loop n =
   let name = name ^ string_of_int n in
   find loop (n+1) name in
 find loop 2 name

module Path_map=
  Map.Make(struct
    type t= string list
    let compare (x:string list) y = compare x y
  end)


type 'a pending_rec_def =
  | Pending: { id: string list; defs: ('a,'a r) rec_defs } -> 'a r pending_rec_def
  | Closed: void pending_rec_def

type dyn = Dyn: 'f pending_rec_def * ('a,'f) s -> dyn

type effective_paths = (string list * int) list Path_map.t
type context = { stamp:int; mapped: effective_paths  }
let id (x: (_,_) s) = Hashtbl.hash x

type def = { desc: string list; ctx: context; map: dyn forest }
let add_path (type a f) path (defs:f pending_rec_def) (x:(a,f) s) {desc;ctx;map} =
  let open L in
  let rec add_path ctx path x map =
    match path with
    | [] -> let name, ctx = arbitrary_name ctx in add_path ctx [name] x map
    | [name] ->
      let name, m = find_nearly name map in
      if m = Name.Map.empty then
        ctx, [name], Name.Map.add name (Item (desc,Dyn (defs,x))) map
      else
        let ctx, q, m = add_path ctx [] x m in
        ctx, name :: q, Name.Map.add name (M m) map
    | a :: q->
      let a, m = find_nearly a map in
      let ctx, q, v = add_path ctx q x m in
      ctx, a :: q, Name.Map.add a (M v) map in
  match Path_map.find path ctx.mapped with
  | exception Not_found ->
    let stamp, effective_path, map = add_path ctx.stamp path x map in
    let  mapped = Path_map.add path [effective_path, id x] ctx.mapped in
    { desc = L.[]; ctx = {stamp;mapped}; map }
  | l when List.exists (fun (_,d) -> (d = id x)) l ->
    { desc = L.[]; ctx; map}
  | l ->
    let stamp, effective_path, map = add_path ctx.stamp path x map in
    let mapped =
      Path_map.add path ( (effective_path, id x) :: l ) ctx.mapped in
    { desc = L.[]; ctx = { stamp; mapped}; map }

let mem (path,x) ctx = match Path_map.find path ctx.mapped with
  | exception Not_found -> false
  | l -> List.exists (fun (_,h) -> h = id x) l


(* For recursive definition, rewire *)
let find_path (path,(x:(_,_) s)) mapped = try
    String.concat "/" @@ fst
    @@ List.find (fun (_,idy) -> id x = idy) @@ Path_map.find path mapped
  with
  | Not_found ->
    Format.eprintf "Unbound schema definition: %a@." Pp.(list ~sep:(const "/") string) path; exit 2

let rec to_int: type l x. (l,x) index -> int = function
  | Zn -> 0
  | Sn x -> 1 + to_int x


let extract_def defs s =
  let rec extract_def:
    type a f. defs:f pending_rec_def -> (a,f) s -> def -> def =
    fun ~defs sch data ->
      let traverse x = extract_def ~defs x in
      match sch with
      | Float -> data | Int -> data | String -> data | Bool -> data | Void -> data
      | Array t -> data |> traverse t
      | Obj [] -> data
      | Obj ( (_,_,x) :: q ) ->
        data |> traverse x |> traverse (Obj q)
      | [] -> data
      | a :: q -> data |> traverse a |> traverse q
      | Sum x -> extract_sum_def ~defs x data
      | Custom{sch; _ } -> traverse sch data
      | Description (_,x) -> traverse x data
      | Rec { id; proj; defs=defs' } ->
        let n = string_of_int @@ to_int proj in
        let p = L.( id @ [n] ) in
        let sch =  get defs' proj in
        if mem (p, sch) data.ctx then data else
          data |> add_path p (Pending {id;defs=defs'}) sch |> extract_def ~defs:(Pending {id;defs=defs'}) sch
      | Var n ->
        let Pending defs as p = defs in
        let path = L.( defs.id @ [string_of_int (to_int n)]) in
        let sch = get defs.defs n in
        if mem (path, sch) data.ctx then data else
          data |> add_path path (Pending defs) sch |> extract_def ~defs:p sch
  and extract_sum_def: type a b. defs:b pending_rec_def ->(a,b) sum_decl -> def -> def =
    fun ~defs s data ->
    match s with
      | [] -> data
      | (_,t) :: q -> data |> extract_def ~defs t |> extract_sum_def ~defs q

  in extract_def ~defs s
    { desc = L.[];
      ctx = {stamp=1;mapped=Path_map.empty};
      map = Name.Map.empty
    }

let pp_descr ppf l =
  if l = L.[] then ()
  else
    Pp.fp ppf {|"description":%S,@ |}
      (String.concat " " @@ List.rev l)

let tyd ppf (dl,typ) = Pp.fp ppf "%a%a" pp_descr dl ty typ

let rec json_type: type a f. effective_paths -> recs: f pending_rec_def
  -> string list -> Format.formatter -> (a,f) s -> unit =
  fun epaths ~recs descr ppf -> function
    | Float -> tyd ppf (descr,"number")
    | Int -> tyd ppf (descr,"number")
    | String -> tyd ppf (descr,"string")
    | Bool -> tyd ppf (descr,"string")
    | Void -> ()
    | Array t -> Pp.fp ppf
                   "%a,@;@[<hov 2>%a : {@ %a@ }@]"
                   tyd (descr,"array") k "items" (json_type ~recs epaths L.[]) t
    | [] -> ()
    | _ :: _ as l ->
      Pp.fp ppf "%a,@; @[<hov 2>%a :[@ %a@ ]@]" tyd (descr,"array") k "items"
        (json_schema_tuple epaths ~recs ) l
    | Obj r ->
      Pp.fp ppf "%a,@;@[<v 2>%a : {@ %a@ }@],@;@[<hov 2>%a@ :@ [@ %a@ ]@]"
        tyd (descr,"object")
        k "properties"
        (json_properties ~recs epaths) r
        k "required"
        (json_required true) r
    | Custom { sch;  _ } -> json_type ~recs epaths descr ppf sch
    | Sum decl ->
      Pp.fp ppf "@[<hov 2>%a%a :[%a]@]"
        pp_descr descr
        k "oneOf" (json_sum ~recs epaths true 0) decl
    | Description (d, sch) -> json_type ~recs epaths L.(d::descr) ppf sch
    | Rec {proj; defs; id } -> json_type ~recs:(Pending { id; defs }) epaths descr ppf (get defs proj)
    | Var n ->
      let Pending recs = recs in 
      let path = L. (recs.id @ [string_of_int (to_int n)] ) in
      let epath = find_path (path, get recs.defs n) epaths in
      Pp.fp ppf {|@["$ref":"#/definitions/%s"@]|} epath
and json_schema_tuple:
  type a f. effective_paths -> recs:f pending_rec_def -> Format.formatter -> (a tuple,f) s -> unit =
  fun epaths ~recs ppf -> function
    | [] -> ()
    | [a] -> Pp.fp ppf {|@[<hov 2>{@ %a@ }@]|} (json_type ~recs epaths L.[]) a
    | a :: q ->
      Pp.fp ppf {|@[<hov 2>{@ %a@ }@],@; %a|}
        (json_type epaths ~recs L.[]) a (json_schema_tuple ~recs epaths) q
    | Description(_, x) -> json_schema_tuple ~recs epaths ppf x
    | Rec {proj; defs; id } -> json_schema_tuple ~recs:(Pending { id; defs }) epaths ppf (get defs proj)
    | Var n ->
      let Pending recs = recs in
      let path = L. (recs.id @ [string_of_int (to_int n)] ) in
      let epath = find_path (path, get recs.defs n) epaths in
      Pp.fp ppf {|@["$ref":#/definitions/%s"@]|} epath
    | Custom _ -> assert false
    | _ -> .
and json_properties:
  type a f. effective_paths -> recs:f pending_rec_def -> Format.formatter -> (a,f) record_declaration -> unit =
  fun epath ~recs ppf -> function
  | [] -> ()
  | [_, n, a] -> Pp.fp ppf {|@[<hov 2>"%s" : {@ %a@ }@]|}
      (show n) (json_type ~recs epath L.[]) a
  | (_, n, a) :: q ->
     Pp.fp ppf {|@[<hov 2>"%s" : {@ %a@ }@],@;%a|}
       (show n) (json_type ~recs epath L.[]) a (json_properties ~recs epath) q
and json_required: type a f. bool ->Format.formatter -> (a,f) record_declaration
  -> unit =
  fun first ppf -> function
  | [] -> ()
  | (Req, n, _) :: q ->
    Pp.fp ppf {|%t"%s"%a|}
      (fun ppf -> if not first then Pp.fp ppf ",@ " else ())
      (show n)
      (json_required false) q
  | _ :: q -> json_required first ppf q
and json_sum:
  type a b. effective_paths -> recs: b pending_rec_def -> bool -> int -> Format.formatter -> (a,b) sum_decl -> unit =
  fun epaths ~recs first n ppf -> function
    | [] -> ()
    | (s, []) :: q  ->
      if not first then Pp.fp ppf ",@,";
      Pp.fp ppf "@[{%a@ :@ [\"%s\"]}@]%a" k "enum" s
        (json_sum ~recs epaths false @@ n + 1) q
    | (s, Void) :: q  ->
      if not first then Pp.fp ppf ",@,";
      Pp.fp ppf "@[{%a@ :@ [\"%s\"]}@]%a" k "enum" s
        (json_sum ~recs epaths false @@ n + 1) q
    | (s,a)::q ->
      if not first then Pp.fp ppf ",@,";
      let module N = Label(struct let l = s end) in
      Pp.fp ppf "{%a}%a" (json_type ~recs epaths L.[]) (Obj[Req,N.l,a])
        (json_sum epaths ~recs false @@ n + 1) q


let json_definitions epaths ppf map =
  let rec json_def ppf name x not_first =
    if not_first then Pp.fp ppf ",@,";
    match x with
    | Item (d, Dyn (ctx,x)) ->
      Pp.fp ppf "@[%a%a@ :@ {@ %a@ }@]" pp_descr d
        k name (json_type ~recs:ctx epaths L.[]) x;
      true
    | M m ->
      Pp.fp ppf "@[%a@ :@ {@ %a@ }@ @]" k name json_defs m; true
  and json_defs ppf m = ignore (Name.Map.fold (json_def ppf) m false) in
  json_defs ppf map

let rec json: type a f. f pending_rec_def -> (a,f) s -> Format.formatter -> a -> unit =
  fun defs sch ppf x -> match sch, x with
    | Int, n -> Pp.fp ppf "%d" n
    | Float, f -> Pp.fp ppf "%f" f
    | String, s -> Pp.estring ppf s
    | Bool, b -> Pp.fp ppf "%b" b
    | Void, _ -> .
    | Array k, l ->
      Pp.fp ppf "@[<hov>[%a]@]"
        (Pp.list ~sep:(Pp.s ",@ ") @@ json defs k) l
    | [], [] -> ()
    | _ :: _ as sch , l -> Pp.fp ppf "@[<hov>[%a]@]" (json_tuple defs sch) l
    | Obj sch, x -> Pp.fp ppf "@[<hv>{@ %a@ }@]" (json_obj false defs sch) x
    | Custom c, x -> json defs c.sch ppf (c.fwd x)
    | Sum q, x -> json_sum 0 defs q ppf x
    | Description(_,sch), x -> json defs sch ppf x
    | Var n, x ->
      let Pending p = defs in
      json defs (get p.defs n) ppf x
    | Rec { defs; id; proj; _ }, x -> json (Pending {defs; id}) (get defs proj) ppf x
and json_sum: type all x. int -> all pending_rec_def -> (x,all) sum_decl  ->
  Format.formatter -> x sum -> unit =
  fun n defs sch ppf x -> match sch, x with
    | (n,a) :: _ , C Z x ->
      let module N = Label(struct let l=n end) in
      json defs (Obj [Req, N.l, a]) ppf (Record.[N.l, x])
    | (n,_) :: _ , C E ->
      json defs String ppf n
    | _ :: q, C S c -> json_sum (n+1) defs q ppf (C c)
    | [], _ -> .

and json_tuple: type a f. f pending_rec_def -> (a tuple,f) s -> Format.formatter -> a tuple -> unit =
  fun defs sch ppf x -> match sch, x with
    | [], [] -> ()
    | [a], [x] -> json defs a ppf x
    | a :: q, x :: xs -> Pp.fp ppf "%a,@ %a" (json defs a) x (json_tuple defs q) xs
    | Custom _, _ -> assert false
    | Description _, _ -> assert false
    | Rec { defs; proj; id; _ }, x -> json_tuple (Pending {id;defs}) (get defs proj) ppf x
    | Var proj, x ->
      let Pending p =  defs in
      json_tuple defs (get p.defs proj) ppf x

and json_obj: type a r.
  bool -> r pending_rec_def -> (a,r) record_declaration -> Format.formatter -> a record -> unit =
  fun not_first defs sch ppf x -> match sch, x with
    | [], [] -> ()
    | (Req, name,sch) :: q ,   (_, x) :: xs ->
      if not_first then Pp.fp ppf ",@ ";
      Pp.fp ppf {|@[<hov 2>"%s"@ :@ %a@]|} (show name) (json defs sch) x;
      Pp.fp ppf "%a" (json_obj true defs q) xs
    | (Opt,name,sch) :: q, (_,Some x) :: xs ->
      if not_first then Pp.fp ppf ",@ ";
      Pp.fp ppf {|@[<hov 2>"%s"@ :@ %a@]|} (show name) (json defs sch) x;
      Pp.fp ppf "%a" (json_obj true defs q) xs
    | (Opt,_,_) :: q, (_, None ) :: xs ->
      json_obj not_first defs q ppf xs


let json x = json Closed x

let cstring ppf s =
  begin try
      ignore(String.index s ' ');
      Pp.estring ppf s
    with
      Not_found -> Pp.string ppf s
  end

let rec sexp: type a all. all pending_rec_def -> (a,all) s -> Format.formatter -> a -> unit =
  fun defs sch ppf x -> match sch, x with
    | Int, n -> Pp.fp ppf "%d" n
    | Float, f -> Pp.fp ppf "%f" f
    | Bool, b -> Pp.fp ppf "%b" b
    | String, s -> cstring ppf s
    | Void, _ -> .
    | Array k, l ->
      Pp.fp ppf "@[<hov>(%a)@]"
        (Pp.list ~sep:(Pp.s "@ ") @@ sexp defs k) l
    | Obj sch, x -> Pp.fp ppf "@[<hov>(@;<1 2>%a@;<1 2>)@]" (sexp_obj defs sch) x
    | [], [] -> ()
    | _ :: _ as tu, t -> Pp.fp ppf "@[<hov>(@;%a@;)@]" (sexp_tuple defs tu) t
    | Custom r, x -> sexp defs r.sch ppf (r.fwd x)
    | Sum s, x -> sexp_sum 0 defs s ppf x
    | Description(_,sch), x -> sexp defs sch ppf x
    | Rec { defs; proj; id }, x ->
      sexp (Pending {defs;id}) (get defs proj) ppf x
    | Var proj, x ->
      let Pending p = defs in
      sexp defs (get p.defs proj) ppf x
and sexp_tuple: type all a. all pending_rec_def -> (a Tuple.t,all) s -> Format.formatter -> a Tuple.t -> unit =
  fun defs ty ppf t -> match ty, t with
    | [], [] -> ()
    | [a], [x] -> sexp defs a ppf x
    | a :: q, x :: xs -> Pp.fp ppf "%a@ %a" (sexp defs a) x (sexp_tuple defs q) xs
    | Custom _, _ -> assert false
    | Description _, _ -> assert false
    | Rec { defs; proj; id }, x ->
      sexp_tuple (Pending {id;defs}) (get defs proj) ppf x
    | Var proj, x ->
      let Pending p = defs in
      sexp_tuple defs (get p.defs proj) ppf x
and sexp_sum: type all a. int -> all pending_rec_def -> (a, all) sum_decl -> Format.formatter ->
  a sum -> unit =
  fun n defs decl ppf x -> match decl, x with
    | (n,a) :: _ , C Z x -> sexp defs [String;a] ppf Tuple.[n;x]
    | (n,_) :: _ , C E -> sexp defs String ppf n
    | _ :: q, C S c -> sexp_sum (n+1) defs q ppf (C c)
    | [] , _ -> .
and sexp_obj: type a all.
  all pending_rec_def -> (a,all) record_declaration -> Format.formatter -> a record -> unit =
  fun defs sch ppf x -> match sch, x with
    | [], [] -> ()
    | (Req, name,sch) :: q ,   (_, x) :: xs ->
      Pp.fp ppf {|(%a@ %a)|} cstring (show name) (sexp defs sch) x;
      begin match q, xs  with
        | [], [] -> ()
        | _ -> Pp.fp ppf "@ %a" (sexp_obj defs q) xs
      end
    | (Opt, name,sch) :: q ,   (_, Some x) :: xs ->
      Pp.fp ppf {|(%a@ %a)|} cstring (show name) (sexp defs sch) x;
      begin match q, xs  with
        | [], [] -> ()
        | _ -> Pp.fp ppf "@ %a" (sexp_obj defs q) xs
      end

    | (Opt,_,_) :: q, (_, None ) :: xs -> sexp_obj defs q ppf xs

let sexp x = sexp Closed x

let skip name = name, None

let ($=?) field x = match x with
  | Some x -> field $= Some x
  | None -> skip field

let obj (x:_ record)= x

module Untyped = struct
type t =
  | Array of t list
  | List of t list
  | Atom of string
  | Obj of (string * t) list
type untyped = t
end
type untyped = Untyped.t =
  | Array of untyped list
  | List of untyped list
  | Atom of string
  | Obj of (string * untyped) list

let promote_to_obj l =
  let promote_pair = function List [Atom x;y] -> Some(x,y) | _ -> None in
  Option.List'.map promote_pair l

let rec retype: type a f. f pending_rec_def -> (a,f) s -> untyped -> a option =
  let open Option in
  fun defs sch u -> match sch, u with
    | Int, Atom u -> Support.opt int_of_string u
    | Float, Atom u -> Support.opt float_of_string u
    | Bool, Atom u -> Support.opt bool_of_string u
    | String, Atom s -> Some s
    | Array t, (Array ul | List ul) ->
      Option.List'.map (retype defs t) ul
    |  [], Array [] -> Some []
    | (a::q), (Array(ua :: uq) | List(ua :: uq)) ->
        retype defs a ua >>= fun h ->
        retype defs q (Array uq) >>| fun q ->
        Tuple.(h :: q)
    | Obj r, Obj ur ->
      retype_obj defs r ur
    | Obj r, List ur ->
      promote_to_obj ur >>= fun obj ->
      retype_obj defs r obj
    | Custom r, x -> retype defs r.sch x >>| r.rev
    | Sum s, Atom x ->
      retype_const_sum s x
    | Sum s, (Obj[x,y]|List[Atom x;y]) ->
      retype_sum defs s x y
    | Rec {defs;proj; id}, x ->
      retype (Pending {defs;id}) (get defs proj) x
    | Var proj, x ->
      let Pending p = defs in
      retype defs (get p.defs proj) x
    | _ -> None
and retype_obj: type a f. f pending_rec_def -> (a,f) record_declaration -> (string * untyped) list ->
  a record option = fun defs sch x ->
  let open Option in
  match sch, x with
  | [], [] -> Some []
  | (Req, field, t) :: q , (ufield, u) :: uq when show field = ufield ->
    retype defs t u >>= fun h ->
    retype_obj defs q uq >>| fun l ->
    Record.( (field $= h) :: l )
  | (Opt, field, t) :: q , (ufield, u) :: uq when show field = ufield ->
    retype defs t u >>= fun h ->
    retype_obj defs q uq >>| fun l ->
    Record.( (field $=? Some h) :: l )
  | (Opt,field,_) :: q , l ->
    retype_obj defs q l >>| fun l ->
    Record.( skip field :: l )
  | _ -> None

and retype_sum: type all a. all pending_rec_def -> (a,all) sum_decl -> string
  -> untyped -> a sum option =
  let open Option in
  fun defs decl n u ->
    match decl with
    | (s, a) :: _  when s = n ->
      retype defs a u >>| fun a -> C(Z a)
    | [] -> None
    |  _ :: q ->
      retype_sum defs q n u >>| fun (C c) -> (C (S c))

and retype_const_sum: type a b. (a,b) sum_decl -> string -> a sum option =
  let open Option in
  fun decl n -> match decl with
    | ( (s, Void) :: _ ) when s = n -> Some (C E)
    | [] -> None
    | _ :: q ->
      retype_const_sum q n >>| fun (C c) -> (C (S c))

let retype x = retype Closed x

let minify ppf =
  let f = Format.pp_get_formatter_out_functions ppf () in
  let space_needed = ref false in
  let out_string s start stop =
    let special c =
      match c with
      | '(' | ',' |'{' | '"' |'[' | ')'| ']'| '}' -> true
      | _ -> false in
    if !space_needed && not (special s.[start]) then
      f.out_string " " 0 1;
    f.out_string s start stop;
    space_needed := not (special s.[stop-1]) in
  let basic = Format_compat.transform f out_string in
  Format.pp_set_formatter_out_functions ppf basic;
  Format.kfprintf (fun _ -> Format.pp_set_formatter_out_functions ppf f;
                    Format.pp_print_flush ppf ()) ppf


let default x y = if x = y then None else Some y

let option (type a f) (sch: (a,f) s) =
  custom (Sum ["None", Void; "Some", sch])
    (function None -> C E | Some x -> C (S(Z x)))
    (function C E -> None | C S Z x -> Some x | C S E -> None |  _ -> . )

let (<?>) x y = Description(y,x)

module Ext = struct

  type ('lbl,'a) ext = {
    title: string;
    description: string;
    version: Version.t;
    label: 'lbl label;
    inner: 'a t;
  }

  type 'a diff = {expected: 'a; got:'a}
  type error =
    | Future_version of Version.t diff
    | Mismatched_kind of string diff
    | Unknown_format
    | Parse_error


  type bound = B: 'a t * 'a -> bound

  let bind sch x = B(sch,x)

  let schema_gen (ext: (_,_) ext): _ t =
    (Obj [ Req, Version.Lbl.l, Version.sch; Req, ext.label, ext.inner ])

  let schema_obj: _ t -> _ t = function
    | Obj r -> Obj ((Req, Version.Lbl.l, Version.sch) :: r)
    | _ -> assert false


  let schema_custom ext =
    match ext.inner with
    | Custom {fwd;rev;sch=(Obj _ as sch)} ->
      let sch = schema_obj sch in
      let rev ( (l,v) :: q :  _ record): _ record =
        [l $= v; ext.label $= rev q] in
      let fwd ([ (lv,v); (_,x)]: _ record) =
        ((lv $= v) :: fwd x: _ record) in
      Custom {fwd; rev; sch}
    | _ -> assert false

  let schema (type a b) (sch: (a,b) ext) = match sch.inner with
    | Obj _ as x -> Dyn (Closed,schema_obj x)
    | Custom { sch = Obj _; _} -> Dyn(Closed,schema_custom sch)
    | _ -> Dyn (Closed,schema_gen sch)

  let extend (type a b) (sch: (a,b) ext) (x: b) = match sch.inner, x with
    | Obj _ as s , x ->
      bind (schema_obj s)
        ((Version.Lbl.l $= sch.version) :: x)
    | Custom { sch = Obj _; _ }, x -> bind (schema_custom sch)
        [Version.Lbl.l $= sch.version;  sch.label $= x ]
    | _, x ->
      bind (schema_gen sch)
        [ Version.Lbl.l $= sch.version; sch.label $= x ]



  let json s ppf x =
    let B (sch, x) = extend s x in
    json sch ppf x

  let json_schema ppf s =
    let Dyn (rctx,sch) = schema s in
    let {ctx; map; _ } = extract_def rctx sch in
    Pp.fp ppf
      "@[<v 2>{@ \
       %a,@;\
       %a,@;\
       %a,@;\
       @[%a :@ {%a},@]@;\
       %a\
       @ }@]@."
      p ("$schema", "http://json-schema.org/schema#")
      p ("title", s.title)
      p ("description", s.description)
      k "definitions" (json_definitions ctx.mapped) map
      (json_type ~recs:rctx ctx.mapped L.[]) sch

  let sexp s ppf x =
    let B(s,x) = extend s x in
    sexp s ppf x

  let optr = function
    | None -> Error Parse_error
    | Some x -> Ok x

  let opt f x y = optr (f x y)
  let rec strict s = let open Mresult.Ok in
    function
    | Obj [ version, v; name, data ]
      when version = show Version.Lbl.l && name = show s.label ->
      opt retype Version.sch v >>= fun v ->
      if v = s.version then
        opt retype s.inner data
      else Error (Future_version { expected=s.version; got=v })
    | List l ->
      optr (promote_to_obj l) >>= fun ol -> strict s (Obj ol)
    | Obj [ version, _; name, _ ] when version = show Version.Lbl.l ->
      Error (Mismatched_kind { expected = show s.label; got = name  } )
    | Array _ | Atom _ | Obj _ -> Error Unknown_format

  type ('a,'b) t = ('a,'b) ext

end
