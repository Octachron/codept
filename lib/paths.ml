
let rec last = function
  | [a] -> a
  | [] -> raise  @@  Invalid_argument "last expected a non-empty-file"
  | _ :: q -> last q

let may_chop_extension a =
  try Filename.chop_extension a with
    Invalid_argument _ -> a

let module_name file =
  String.capitalize_ascii @@ may_chop_extension @@ last @@ file


module Simple =
struct
  module Core = struct
    type t = Name.t list

    let compare (x:t) (y:t) = compare x y
    let pp = Pp.(list ~sep:(s".") ) Name.pp
  end
  include Core
  let sexp = Sexp.( list string )
  let sch = Scheme.(Array String)

  module Set = struct
    include Set.Make(Core)
    let pp ppf s = Pp.(clist Core.pp) ppf (elements s)
  end
  module Map = struct
    include (Map.Make(Core))
    let find_opt k m = try Some(find k m) with Not_found -> None
    let union' s = union (fun _key _m1 m2 -> Some m2) s
  end
  type set = Set.t
  type 'a map = 'a Map.t
  let prefix = List.hd

  let extension a =
    let ext = Support.extension a in
    if not (ext = "") && ext.[0] = '.' then
      String.sub ext 1 (String.length ext - 1)
    else
      ext

  let may_change_extension f a =
    match extension a with
    | "" -> a
    | ext ->
      let base = Filename.chop_extension a in
      base ^ f ext

  let rec change_file_extension f = function
    | [] -> []
    | [a] -> [may_change_extension f a ]
    | a :: q -> a :: change_file_extension f q

  let rec chop_extension l = match l with
    | [] -> []
    | [a] -> [Filename.chop_extension a]
    | a :: q -> a :: chop_extension q

  let parse_filename name =
    let l = Support.split_on_char (String.get (Filename.dir_sep) 0) name in
    match List.rev l with
    | "" :: q -> List.rev q
    | l -> List.rev l

  let module_name = module_name

end
module S = Simple

module Expr = struct
  type t =
    | T
    | A of Name.t
    | S of t * Name.t
    | F of {f:t; x:t}


  module Sexp = struct
    open Sexp
    let t = simple_constr "T" T
    let a = C {name="A";
               proj = (function A x -> Some x | _ -> None);
               inj = (fun x -> A x);
               impl = string;
               default = None
              }
    let s r =
      C {name="S";
         proj = (function S (x,y) -> Some (x,y) | _ -> None);
         inj = (fun (x,y) -> S (x,y) );
         impl = pair (fix' r) string;
         default = None
        }

    let f r = C {name="F";
                 proj = (function F {f;x} -> Some (f, x) | _ -> None);
                 inj = (fun (f,x) -> F {f;x} );
                 impl = pair (fix' r) (fix' r);
                 default = None;
                }

    let rec all () = sum [ t; a; s all; f all]
    let all = all ()
  end
  let sexp = Sexp.all

  module Sch = struct
    open Scheme
    let rec raw = Sum[ Void; String; [t;String]; [t; t] ]
    and t = Custom {fwd;rev; sch=raw; id = "Paths.Expr.t" }
    and fwd = let open Tuple in
      function
      | T -> C E
      | A s -> C (S (Z s))
      | S(t,s) -> C(S(S(Z [t;s])))
      | F {f;x} -> C(S(S(S(Z [f;x]))))
    and rev = let open Tuple in
      function
      | C E -> T
      | C S Z s -> A s
      | C S S Z [t;s] -> S(t,s)
      | C S S S Z [f;x] -> F {f;x}
      | _ -> .
  end
  let sch = Sch.t


  exception Functor_not_expected
  let concrete p: Simple.t =
    let rec concretize l = function
      | T -> l
      | A a -> a :: l
      | F _ -> raise Functor_not_expected
      | S(p,s) -> concretize (s::l) p in
    concretize [] p

  let concrete_with_f p: Simple.t =
    let rec concretize l = function
      | T -> l
      | A a -> a :: l
      | F {f;_} -> concretize l f
      | S(p,s) -> concretize (s::l) p in
    concretize [] p


  let multiples p : Simple.t list =
    let rec concretize stack l = function
      | T -> l :: stack
      | A a -> (a :: l) :: stack
      | F {f;x} -> concretize (concretize stack [] x) l f
      | S(p,s) -> concretize stack (s::l) p in
    concretize [] [] p

  let rev_concrete p = List.rev @@ concrete p

  let from_list l =
    let rec rebuild =
      function
      | [] -> T
      | [a] -> A a
      | a :: q -> S(rebuild q, a)
    in rebuild @@ List.rev l

  let rec pp ppf =
    let p fmt = Format.fprintf ppf fmt in
    function
    | T -> p "T"
    | A name -> p"%s" name
    | S(h,n) -> p "%a.%s" pp h n
    | F {f;x} -> p "%a(%a)" pp f pp x

  let rec prefix = function
    | S(p,_) -> prefix p
    | A n -> n
    | F {f;_} -> prefix f
    | T -> raise @@ Invalid_argument "Paths.Expr: prefix of empty path"
end
module E = Expr

module Pkg = struct
  type source = Local | Unknown | Pkg of Simple.t | Special of Name.t

  let sep = Filename.dir_sep

  type t = { source: source ; file: Simple.t }
  type path = t

  module Sexp = struct
    open Sexp
    let local = simple_constr "Local" Local
    let unknown = simple_constr "Unknwon" Unknown
    let pkg = C {name="Pkg";
                 proj= (function Pkg p -> Some p | _ -> None );
                 inj = (fun x -> Pkg x);
                 impl = Simple.sexp;
                 default = Some [];
                }
    let special =
      C {name="Special";
         proj= (function Special s -> Some s | _ -> None );
         inj = (fun x -> Special x);
         impl = string;
         default = Some "stdlib"
        }
    let source = sum [local;unknown;pkg;special]

    let ksource = Record.(key One_and_many "source" Local)
    let file = Record.(key Many "file" [])

    let all =
      convr Record.(define [field ksource source ; field file Simple.sexp ])
        ( fun x -> let get f = Record.get f x in
          { source = get ksource; file = get file }
        )
        (fun r -> Record.(create [ksource := r.source; file := r.file]))

  end
  let sexp = Sexp.all

  module Sch = struct open Scheme
    let raw_source = Sum [ Void; Void; Simple.sch; String ]
    let source = custom "Paths.Pkg.source" raw_source
        (function
          | Local -> C E
          | Unknown -> C (S E)
          | Pkg s -> C (S (S (Z s)))
          | Special s -> C(S(S(S(Z s))))
        )
        (function
          | C E -> Local
          | C S E -> Unknown
          | C S S Z s -> Pkg s
          | C S S S Z s -> Special s
          | _ -> .
        )
    let all =
        custom "Paths.Pkg.t" [source; Simple.sch]
          (fun {source;file} -> Tuple.[source;file])
          Tuple.(fun [source;file] ->  {source;file} )
  end let sch = Sch.all

  let filename ?(sep=sep) p =
    begin match p.source with
      | Pkg n -> String.concat sep n ^ sep
      | _ -> ""
    end
    ^
    String.concat sep p.file

  let is_known = function
    | {source=Unknown; _ } -> false
    | _ -> true

  let module_name {file; _ } = module_name file

  let update_extension f p =
    { p with file = Simple.change_file_extension f p.file }

  let change_extension ext =
    update_extension ( fun _ -> ext )

  let cmo = change_extension ".cmo"
  let o = change_extension ".o"
  let cmi = change_extension ".cmi"
  let cmx = change_extension ".cmx"
  let cmxs = change_extension ".cmx"

  let mk_dep all native = update_extension @@ function
    | "mli" | "m2li" -> ".cmi"
    | "ml" | "m2l" when all -> ".cmi"
    | "ml" | "m2l" ->
      if native then ".cmx" else ".cmo"
    | "cmi" -> ".cmi"
    | s -> raise @@Invalid_argument ("Unknown extension " ^ s)

  let pp_source ppf = function
    | Local -> Pp.fp ppf "Local"
    | Unknown ->  Pp.fp ppf "Unknown"
    | Pkg n -> Pp.fp ppf "Pkg [%a]" Pp.(list ~sep:(const sep) string) n
    | Special s -> Pp.fp ppf "Special %s" s


  let pp_simple ppf {source;file}=
    Pp.fp ppf "(%a)%a" pp_source source
      Pp.(list ~sep:(const sep) string) file

  let pp_gen sep ppf {source;file} =
    begin match source with
      | Local -> ()
      | Unknown -> Pp.fp ppf "?"
      | Pkg s ->
        Pp.fp ppf "%a%s"
          Pp.(list ~sep:(const sep) string) s
          sep
      | Special s -> Pp.fp ppf "(*%s*)" s
    end;
    Pp.fp ppf "%a"
      Pp.(list ~sep:(const sep) string) file

  let pp = pp_gen sep
  let es ppf = Pp.fp ppf {|"%s"|}

  let reflect_source ppf =
    function
    | Local -> Pp.fp ppf "Local"
    | Unknown ->  Pp.fp ppf "Unknown"
    | Pkg n -> Pp.fp ppf "Pkg [%a]" Pp.(list ~sep:(s "; ") es) n
    | Special n -> Pp.fp ppf "Special %a" es n

  let reflect ppf {source;file} =
    Pp.fp ppf "{source=%a; file=[%a]}"
      reflect_source source
      Pp.(list ~sep:(const "; ") es) file

  module Set = struct
    include Set.Make(struct type t = path let compare = compare end)
    let pp ppf s = Pp.(clist pp) ppf (elements s)
  end
  module Map = struct
    include Map.Make(struct type t = path let compare = compare end)
    let find_opt k m = try Some(find k m) with Not_found -> None
  end
  type set = Set.t
  type 'a map = 'a Map.t

  let local file = { source = Local; file = S.parse_filename file }
  let (/) simple {source; file} = {source; file = simple @ file }

end
module P = Pkg
