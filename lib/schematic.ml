
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

(*
type ('m,'a) elt =
  | Just: 'a -> ('any,'a) elt
  | Nothing: (optional,'any) elt
*)


module Record = struct
  type 'a record =
    | []: void record
    | (::): ( 'a label * 'elt) * 'c record ->
      ('a * 'elt * 'c) record
  type 'a t = 'a record
end
open Record

type ('a,'b) bijection = { fwd:'a->'b;rev:'b -> 'a}

type ('hole, 'free) t =
  | Float: (float, 'free) t
  | Int: (int, 'free) t
  | Bool: (bool, 'free) t
  | String: (string, 'free) t
  | Void: (void, 'free) t
  | Array: ('hole,'free) t -> ('hole list, 'free) t
  | (::) : ('a,'free) t * ('b tuple, 'free) t -> (('a * 'b) tuple, 'free) t
  | []: (void tuple, 'free) t
  | Obj: ('a,'free) record_declaration -> ('a record, 'free) t
  | Custom: ('a,'b,'free) custom -> ('a, 'free) t
  | Sum: ('a,'free) sum_decl -> ('a sum,'free) t
  | Description: string * ('hole,'free) t -> ('hole, 'free) t
  | Rec: { id: string list; defs:('defs,'defs) rec_defs; proj: ('defs, 'res) index}
      -> ('res,'free) t
  | Var: ('free,'result) index -> ('result,'free) t

and (_,_) index =
  | Zn: ('a * 'b ,'a) index
  | Sn: ('list,'res) index -> ( _ * 'list, 'res) index

and (_,_) rec_defs =
  | []: (void,'free) rec_defs
  | (::): ('a,'free) t * ('l, 'free) rec_defs -> ('a * 'l, 'free) rec_defs

and ('a,'b,'free) custom = { fwd:'a -> 'b; rev:'b -> 'a; sch:('b,'free) t; id:string list }
and ('a,'free) record_declaration =
  | []: (void, 'free) record_declaration
  | (::): ( ('m,'x,'fx) modal * 'a label * ('x,'free) t) * ('c,'free) record_declaration
    -> (  'a * 'fx * 'c, 'free) record_declaration

and ('a,'mu) sum_decl =
    | [] : (<before:void>, 'mu) sum_decl
    | (::): (string * ('a,'mu) t) * ('b,'mu) sum_decl
        -> (<at:'a; before:'b>,'mu) sum_decl

and (_,_) cons =
  | Z: 'a -> (<at:'a; before: 'any>,'a) cons
  | E: (<at:void; before:'any>,'a) cons
  | S: ('a, 'n ) cons -> (<at:'any; before:'a>, 'n) cons

and 'a sum = C: ('a, 'elt ) cons -> 'a sum

type 'a schematic = ('a,void) t

type (_,_,_,_) n =
  | Z: ('a * 'l, void, 'a, 'l) n
  | S: ('all, 'l, 'at, 'x * 'r) n -> ('all, 'at * 'l, 'x, 'r) n

let rec get: type a b all l f r. (a,all) rec_defs -> (all,b,r,a) n -> (r,all) t =
  fun defs n ->
  match defs, n with
  | a::_, Z -> a
  | _ :: q, S x -> get q x
  | _ -> .


let rec get: type a all res. (a,all) rec_defs -> (a,res) index -> (res,all) t = fun l x ->
  match l, x with
  | a::_ , Zn -> a
  | _ :: q, Sn x -> get q x
  | [], _ -> .


let rec reopen: type a b. (a,void) t -> (a,b) t =
  function
  | Float -> Float
  | Int -> Int
  | Void -> Void
  | Bool -> Bool
  | String -> String
  | Var _ -> .
  | Rec {id;proj;defs} -> Rec {id;proj;defs}
  | [] -> []
  | x :: q -> reopen x :: reopen q
  | Array x -> Array (reopen x)
  | Obj q -> Obj (reopen_obj q)
  | Custom {rev;id;fwd;sch} -> Custom {rev;id;fwd;sch=reopen sch}
  | Sum x -> Sum (reopen_sum x)
  | Description(d,x) -> Description(d, reopen x)
and reopen_obj: type a f. (a,void) record_declaration -> (a,f) record_declaration = function
  | [] -> []
  | (Req, lbl, a) :: q -> (Req, lbl, reopen a) :: reopen_obj q
  | (Opt, lbl, a) :: q -> (Opt, lbl, reopen a) :: reopen_obj q
and reopen_sum: type a f. (a,void) sum_decl -> (a,f) sum_decl = function
    | (name, a) :: q -> (name, reopen a) :: reopen_sum q
    | [] -> []

let custom id sch fwd rev = Custom {fwd;rev; sch; id}
module Version = struct
  type t = { major:int; minor:int; patch:int }
  module Lbl = Label(struct let l = "version" end)
  type lbl = Lbl.t

  let sch =
  custom ["version"] [Int;Int;Int]
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
type dyn = Dyn: ('f,'f) rec_defs * ('a,'f) t -> dyn
type effective_paths = (string list * int) list Path_map.t
type context = { stamp:int; mapped: effective_paths  }
let id x = Hashtbl.hash x

type def = { desc: string list; ctx: context; map: dyn forest }
let add_path (type a f) path (defs:(f,f) rec_defs) (x:(a,f) t) {desc;ctx;map} =
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

let find_path (path,x) mapped =
  String.concat "/" @@ fst
  @@ List.find (fun (_,idy) -> id x = idy) @@ Path_map.find path mapped




let extract_def defs s =
  let rec extract_def:
    type a f. (f,f) rec_defs -> (a,f) t -> def -> def =
    fun defs sch data -> match sch with
      | Float -> data | Int -> data | String -> data | Bool -> data | Void -> data
      | Array t -> data |> extract_def defs t
      | Obj [] -> data
      | Obj ( (_,_,x) :: q ) ->
        data |> extract_def defs x |> extract_def defs (Obj q)
      | [] -> data
      | a :: q -> data |> extract_def defs a |> extract_def defs q
      | Sum x -> extract_sum_def defs x data
      | Custom{id; sch; _ } ->
        if mem (id,sch) data.ctx then data
        else data |> add_path id defs sch |> extract_def defs sch
      | Description (_,x) -> extract_def defs x data
      | Rec { id; proj; defs=defs' } ->
        if mem (id, sch) data.ctx then data else
          data |> add_path id defs sch |> extract_def defs' (get defs' proj)
      | Var k ->  extract_def defs (get defs k) data
  and extract_sum_def: type a b. (b,b) rec_defs ->(a,b) sum_decl -> def -> def =
    fun defs s data ->
    match s with
      | [] -> data
      | (_,t) :: q -> data |> extract_def defs t |> extract_sum_def defs q

  in extract_def defs s
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

let rec json_type: type a f. effective_paths
  -> string list -> Format.formatter -> (a,f) t -> unit =
  fun epaths descr ppf -> function
    | Float -> tyd ppf (descr,"number")
    | Int -> tyd ppf (descr,"number")
    | String -> tyd ppf (descr,"string")
    | Bool -> tyd ppf (descr,"string")
    | Void -> ()
    | Array t -> Pp.fp ppf
                   "%a,@;@[<hov 2>%a : {@ %a@ }@]"
                   tyd (descr,"array") k "items" (json_type epaths L.[]) t
    | [] -> ()
    | _ :: _ as l ->
      Pp.fp ppf "%a,@; @[<hov 2>%a :[@ %a@ ]@]" tyd (descr,"array") k "items"
        (json_schema_tuple epaths) l
    | Obj r ->
      Pp.fp ppf "%a,@;@[<v 2>%a : {@ %a@ }@],@;@[<hov 2>%a@ :@ [@ %a@ ]@]"
        tyd (descr,"object")
        k "properties"
        (json_properties epaths) r
        k "required"
        (json_required true) r
    | Custom { id ; sch;  _ } ->
      Pp.fp ppf "@[<hov 2>%a@ :@ \"#/definitions/%s\"@]" k "$ref"
        (find_path (id,sch) epaths)
    | Sum decl ->
      Pp.fp ppf "@[<hov 2>%a%a :[%a]@]"
        pp_descr descr
        k "oneOf" (json_sum epaths true 0) decl
    | Description (d, sch) -> json_type epaths L.(d::descr) ppf sch
    | Rec {proj; defs; _ } -> json_type epaths descr ppf (get defs proj)
    | Var _ -> assert false (* TODO REC schema *)
and json_schema_tuple:
  type a f. effective_paths -> Format.formatter -> (a tuple,f) t -> unit =
  fun epath ppf -> function
    | [] -> ()
    | [a] -> Pp.fp ppf {|@[<hov 2>{@ %a@ }@]|}
               (json_type epath L.[]) a
    | a :: q ->
      Pp.fp ppf {|@[<hov 2>{@ %a@ }@],@; %a|}
        (json_type epath L.[]) a (json_schema_tuple epath) q
    | Custom _ -> assert false
    | Description _ -> assert false
    | Rec {proj; defs; _ } -> json_schema_tuple epath ppf (get defs proj)
    | Var _ -> assert false (* TODO REC schema *)
and json_properties:
  type a f. effective_paths -> Format.formatter -> (a,f) record_declaration -> unit =
  fun epath ppf -> function
  | [] -> ()
  | [_, n, a] -> Pp.fp ppf {|@[<hov 2>"%s" : {@ %a@ }@]|}
      (show n) (json_type epath L.[]) a
  | (_, n, a) :: q ->
     Pp.fp ppf {|@[<hov 2>"%s" : {@ %a@ }@],@;%a|}
       (show n) (json_type epath L.[]) a (json_properties epath) q
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
  type a b. effective_paths -> bool -> int -> Format.formatter -> (a,b) sum_decl -> unit =
  fun epaths first n ppf -> function
    | [] -> ()
    | (s, []) :: q  ->
      if not first then Pp.fp ppf ",@,";
      Pp.fp ppf "@[{%a@ :@ [\"%s\"]}@]%a" k "enum" s
        (json_sum epaths false @@ n + 1) q
    | (_s,_a)::_q -> assert false (*
      if not first then Pp.fp ppf ",@,";
      let module N = Label(struct let l = s end) in
      Pp.fp ppf "{%a}%a" (json_type epaths L.[]) (Obj[Req,N.l,a])
        (json_sum epaths false @@ n + 1) q
    | (_s,Mu)::_q -> assert false*)


let json_definitions epaths ppf map =
  let rec json_def ppf name x not_first =
    if not_first then Pp.fp ppf ",@,";
    match x with
    | Item (d, Dyn (_ctx,x)) ->
      Pp.fp ppf "@[%a%a@ :@ {@ %a@ }@]" pp_descr d
        k name (json_type epaths L.[]) x;
      true
    | M m ->
      Pp.fp ppf "@[%a@ :@ {@ %a@ }@ @]" k name json_defs m; true
  and json_defs ppf m = ignore (Name.Map.fold (json_def ppf) m false) in
  json_defs ppf map

let rec json: type a f. (f,f) rec_defs -> (a,f) t -> Format.formatter -> a -> unit =
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
    | Var n, x -> json defs (get defs n) ppf x
    | Rec { defs; proj; _ }, x -> json defs (get defs proj) ppf x
and json_sum: type all x all. int -> (all,all) rec_defs -> (x,all) sum_decl  ->
  Format.formatter -> x sum -> unit =
  fun n defs sch ppf x -> match sch, x with
    | (n,a) :: _ , C Z x ->
      let module N = Label(struct let l=n end) in
      json defs (Obj [Req, N.l, a]) ppf (Record.[N.l, x])
    | (n,_) :: _ , C E ->
      json defs String ppf n
    | _ :: q, C S c -> json_sum (n+1) defs q ppf (C c)
    | [], _ -> .

and json_tuple: type a f. (f,f) rec_defs -> (a tuple,f) t -> Format.formatter -> a tuple -> unit =
  fun defs sch ppf x -> match sch, x with
    | [], [] -> ()
    | [a], [x] -> json defs a ppf x
    | a :: q, x :: xs -> Pp.fp ppf "%a,@ %a" (json defs a) x (json_tuple defs q) xs
    | Custom _, _ -> assert false
    | Description _, _ -> assert false
    | Rec { defs; proj; _ }, x -> json_tuple defs (get defs proj) ppf x
    | Var proj, x -> json_tuple defs (get defs proj) ppf x

and json_obj: type a r.
  bool -> (r,r) rec_defs -> (a,r) record_declaration -> Format.formatter -> a record -> unit =
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



let ctx_json = json
let json x = json [] x

let cstring ppf s =
  begin try
      ignore(String.index s ' ');
      Pp.estring ppf s
    with
      Not_found -> Pp.string ppf s
  end

let rec sexp: type a all. (all,all) rec_defs -> (a,all) t -> Format.formatter -> a -> unit =
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
    | Rec { defs; proj; _ }, x ->
      sexp defs (get defs proj) ppf x
    | Var proj, x -> sexp defs (get defs proj) ppf x
and sexp_tuple: type all a. (all,all) rec_defs -> (a Tuple.t,all) t -> Format.formatter -> a Tuple.t -> unit =
  fun defs ty ppf t -> match ty, t with
    | [], [] -> ()
    | [a], [x] -> sexp defs a ppf x
    | a :: q, x :: xs -> Pp.fp ppf "%a@ %a" (sexp defs a) x (sexp_tuple defs q) xs
    | Custom _, _ -> assert false
    | Description _, _ -> assert false
    | Rec { defs; proj; _ }, x ->
      sexp_tuple defs (get defs proj) ppf x
    | Var proj, x -> sexp_tuple defs (get defs proj) ppf x
and sexp_sum: type all a. int -> (all,all) rec_defs -> (a, all) sum_decl -> Format.formatter ->
  a sum -> unit =
  fun n defs decl ppf x -> match decl, x with
    | (n,a) :: _ , C Z x -> sexp defs [String;a] ppf Tuple.[n;x]
    | (n,_) :: _ , C E -> sexp defs String ppf n
    | _ :: q, C S c -> sexp_sum (n+1) defs q ppf (C c)
    | [] , _ -> .
and sexp_obj: type a all.
  (all,all) rec_defs -> (a,all) record_declaration -> Format.formatter -> a record -> unit =
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

let sexp x = sexp [] x

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

let rec retype: type a f. (f,f) rec_defs -> (a,f) t -> untyped -> a option =
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
    | Rec {defs;proj; _}, x ->
      retype defs (get defs proj) x
    | Var proj, x ->
      retype defs (get defs proj) x
    | _ -> None
and retype_obj: type a f. (f,f) rec_defs -> (a,f) record_declaration -> (string * untyped) list ->
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

and retype_sum: type all a. (all,all) rec_defs -> (a,all) sum_decl -> string
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

let retype x = retype [] x

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

let option (type a f) name (sch:(a,void) t) =
  custom ["Option.";name] (Sum ["None", Void; "Some", sch])
    (function None -> C E | Some x -> C (S(Z x)))
    (function C E -> None | C S Z x -> Some x | C S E -> None |  _ -> . )

let (<?>) x y = Description(y,x)

module Ext = struct

  type ('lbl,'a,'f) ext = {
    title: string;
    description: string;
    version: Version.t;
    label: 'lbl label;
    inner: ('a,'f) t;
  }

  type 'a diff = {expected: 'a; got:'a}
  type error =
    | Future_version of Version.t diff
    | Mismatched_kind of string diff
    | Unknown_format
    | Parse_error


  type bound = B: ('a,void) t * 'a -> bound

  let bind sch x = B(sch,x)

  let schema_gen (ext: (_,_,_) ext): _ t =
    (Obj [ Req, Version.Lbl.l, Version.sch; Req, ext.label, ext.inner ])

  let schema_obj: _ t -> _ t = function
    | Obj r -> Obj ((Req, Version.Lbl.l, Version.sch) :: r)
    | _ -> assert false


  let schema_custom ext =
    match ext.inner with
    | Custom {fwd;rev;sch=(Obj _ as sch);id} ->
      let sch = schema_obj sch in
      let rev ( (l,v) :: q :  _ record): _ record =
        [l $= v; ext.label $= rev q] in
      let fwd ([ (lv,v); (_,x)]: _ record) =
        ((lv $= v) :: fwd x: _ record) in
      Custom {fwd; rev; sch; id}
    | _ -> assert false

  let schema (type a b c) (sch: (a,b,void) ext) = match sch.inner with
    | Obj _ as x -> Dyn ([],schema_obj x)
    | Custom { sch = Obj _; _} -> Dyn([],schema_custom sch)
    | _ -> Dyn ([],schema_gen sch)

  let extend (type a b c) (sch: (a,b,void) ext) (x: b) = match sch.inner, x with
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
    let Dyn (ctx,sch) = schema s in
    let {ctx; map; _ } = extract_def ctx sch in
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
      (json_type ctx.mapped L.[]) sch

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

  type ('a,'b,'f) t = ('a,'b,'f) ext

end
