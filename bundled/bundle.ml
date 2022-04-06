open Module
open Sig

let version () = Scanf.sscanf Sys.ocaml_version "%d.%d" (fun x y -> x, y)


let top f x = Dict.of_list @@ List.map f x

let root ?(special="stdlib") ?(nms=["Stdlib"])  ?(mds=[]) ?(mts=[]) name =
  let path = Namespaced.make ~nms name in
  let origin =
    Origin.Unit {source={source=Special special; file=path}; path} in
  name, Sig { origin; signature=of_lists mds mts}


let simple ?special ?nms name =
  root ?special ?nms name

let submodule ?special:_ ?nms:_ ?(mds=[]) ?(mts=[]) name =
  name, Sig { origin=Submodule; signature=of_lists mds mts}

let mkfunctor ?(sg=empty) f xs =
  let arg name = Some {Arg.name=Some name; signature=Sig { origin=Submodule; signature=empty}} in
  let fn = List.fold_right (fun a x -> Fun(arg a,x)) xs (Sig {origin=Submodule; signature=sg}) in
  f, fn

let weak =
  root "Weak"
    ~mds:[mkfunctor "Make" ["H"]]
    ~mts:[submodule "S"]

let ephemeron v =
  let fn name = mkfunctor name ["H"] in
  let genhastable = submodule "GenHashTable" ~mds:[fn "MakeSeeded"] in
  let ksub fn =
    let base = [fn "Make"; fn "MakeSeeded"] in
    if v < (4,14) then base else submodule "Bucket" :: base
  in
  let k n = submodule ("K"^n) ~mds:(ksub fn) in
  let k2 =
    let fn n = mkfunctor n ["H1";"H2"] in
    submodule "K2" ~mds:(ksub fn)
  in
  root "Ephemeron"
    ~mds:[genhastable; k "1"; k2; k "n"]
    ~mts:[submodule "S"; submodule "SeededS"]



let chain name sub = root name ~mds:(List.map submodule sub)

let pervasives =  chain "Pervasive" ["LargeFile"]

let obj v =
  if v >= (4, 14) then
    chain "Obj" ["Closure"; "Ephemeron"; "Extension_constructor"]
  else
    chain "Obj" ["Ephemeron"]
let printexc = chain "Printexc" ["Slot"]
let random = chain "Random" ["State"]
let scanf = chain "Scanf" ["Scanning"]
let array version =
  if version >= (4,06) then chain "Array" ["Floatarray"] else
    simple "Array"

let array_labels version =
  if version >= (4,06) then chain "ArrayLabels" ["Floatarray"] else
    simple "ArrayLabels"

let gc v = if v <= (4,12) || v >= (5, 0) then simple "Gc" else chain "Gc" ["Memprof"]

let chained version = [ gc version; array version; array_labels version; obj version; printexc; random;scanf ]

let simples =
  List.map simple
    [ "Arg"; "Buffer"; "Bytes"; "BytesLabels";
      "Callback";
      "CamlinternalOO"; "CamlinternalLazy"; "CamlinternalFormat";
      "CamlinternalFormatBasics"; "CamlinternalMod";
      "Char"; "Complex"; "Digest"; "Filename"; "Format";
      "Genlex"; "Int32"; "Int64"; "Lazy"; "Lexing"; "List";
      "ListLabels"; "Marshal"; "Nativeint"; "Oo"; "Parsing"; "Printf"; "Queue";
      "Stack"; "Stream"; "String"; "StringLabels"; "Uchar"]

type 'p mk = ?special:string -> ?nms:Namespaced.p -> ?mds:'a -> ?mts:'b -> 'c
  constraint 'p = 'a * 'b * 'c
let hashtbl (mk: _ mk)  =
  mk "Hashtbl"
    ~mds:[mkfunctor "Make" ["H"]; mkfunctor "MakeSeeded" ["H"]]
    ~mts:[submodule "HashedType"; submodule "S";
     submodule "SeededHashedType"; submodule "SeededS";
    ]

let map (mk: _ mk) =
  mk "Map"
    ~mds:[mkfunctor "Make" ["Ord"]]
    ~mts:[submodule "OrderedType"; submodule "S"]

let set (mk: _ mk) =
  mk "Set"
    ~mds:[mkfunctor "Make" ["Ord"]]
    ~mts:[submodule "OrderedType"; submodule "S"]

let immediate64 =
  submodule  "Immediate64"
    ~mds:[mkfunctor "Make" ["Immediate"; "Non_immediate"] ]
    ~mts:[submodule "Immediate"; submodule "Non_immediate"]

let sys = root "Sys" ~mds:[immediate64]

let alias suffix name  = name, Alias {path=Namespaced.make (name ^ suffix);phantom=None}

let stdlabels =
  root "StdLabels"
    ~mds:(List.map (alias "Labels") ["Array"; "List"; "Bytes"; "String"])

let spacetime = root "Spacetime"
   ~mds:[submodule "Series"; submodule "Snapshot"]


let hms_base = [hashtbl; map; set]
let more_labels =
  root "MoreLabels"
    ~mds:(List.map ((|>) submodule) hms_base)

let hms = List.map ((|>) root) hms_base

let before version v x = if version < v then x else []
let after version v x = if version >= v then x else []
let in_between version v1 v2 x = if version >= v1 && version < v2 then x else []

let bigarray ?special ?nms () =
  root ?special ?nms "Bigarray"
    ~mds:[
      submodule "Array0";
      submodule "Array1";
      submodule "Array2";
      submodule "Array3";
      submodule "Genarray";
    ]

let complex v =
  after v (4,10) [sys] @ weak :: hms @ more_labels :: stdlabels :: chained v


let float version = if version < (4,08) then simple "Float" else
    chain "Float" ["Array"; "ArrayLabels"]

let simple_stdlib v = Dict.of_list @@
  before v (4,08) [pervasives; simple "Sort"]
  @ after v (4,03) [ephemeron v]
  @ in_between v (4,04) (4,12) [spacetime]
  @ after v (4,07) [bigarray ();float v; simple "Seq"]
  @ after v  (4,08)
    (List.map simple ["Fun";"Bool";"Option";"Int";"Result";"LargeFile"; "Unit"])
  @ before v  (4,10) [simple "Sys"]
  @ after v (4,12) [simple "Either"]
  @ after v (4,13) [simple "Atomic"; simple "CamlinternalAtomic"]
  @ after v (4,14) [simple "In_channel"; simple "Out_channel"]
  @ complex v
  @ simples

let versioned_stdlib v  = ["Stdlib", Namespace (simple_stdlib v)]

let stdlib = Dict.of_list @@ versioned_stdlib @@ version ()


let num =
  top (simple ~special:"stdlib/num" ~nms:[])
    ["Arith_flags"; "Arith_status"; "Big_int"; "Int_misc"; "Nat"; "Num";
     "Ratio"]

let bigarray = Dict.of_list [bigarray ~special:"stdlib/bigarray" ~nms:[] ()]

let unix =
  top (fun x -> root ~special:"stdlib/unix" ~nms:[] x ~mds:[submodule "Largefile"] )
  [ "Unix"; "UnixLabels"]

let graphics =
  top (fun x -> root ~special:"stdlib/graphics" ~nms:[] x) [ "Graphics";"GraphicsX11"]

let threads =
  top (fun x -> root ~special:"stdlib/threads" ~nms:[] x)
    [ "Condition"; "Event"; "Mutex"; "Thread"; "ThreadUnix"]

let dynlink = Dict.of_list [root ~special:"stdlib/dynlink" ~nms:[] "Dynlink"]
