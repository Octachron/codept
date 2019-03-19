open Module
open Sig

let version = Scanf.sscanf Sys.ocaml_version "%d.%d" (fun x y -> x, y)


let top f x = Dict.of_list @@ List.map f x

let root ?(nms="stdlib") ?(mds=[]) ?(mts=[]) name =
  let origin =
    Origin.Unit {source={source=Special nms; file=[name]}; path=[name]} in
  M { name; origin; args=[]; signature=of_lists mds mts}


let simple ?nms name =
  root ?nms name

let submodule ?nms:_ ?(mds=[]) ?(mts=[]) name =
  M {name; origin=Submodule; args=[]; signature=of_lists mds mts}

let mkfunctor ?(sg=empty) f xs =
  let arg name = Some {name; origin=Arg; args=[]; signature=empty} in
  let args = List.map arg xs in
  M {name=f; origin=Submodule; args; signature=sg}

let weak =
  root "Weak"
    ~mds:[mkfunctor "Make" ["H"]]
    ~mts:[submodule "S"]

let ephemeron =
  let fn name = mkfunctor name ["H"] in
  let genhastable = submodule "GenHashTable" ~mds:[fn "MakeSeeded"] in
  let k n = submodule ("K"^n) ~mds:[fn "Make"; fn "MakeSeeded"] in
  let k2 = let fn n = mkfunctor n ["H1";"H2"] in
    submodule "K2" ~mds:[fn "Make"; fn "MakeSeeded"] in
  root "Ephemeron"
    ~mds:[genhastable; k "1"; k2; k "n"]
    ~mts:[submodule "S"; submodule "SeededS"]



let chain name sub = root name ~mds:[submodule sub]

let pervasives =  chain "Pervasive" "LargeFile"

let obj = chain "Obj" "Ephemeron"
let printexc = chain "Printexc" "Slot"
let random = chain "Random" "State"
let scanf = chain "Scanf" "Scanning"
let array =
  if version >= (4,06) then chain "Array" "Floatarray" else
    simple "Array"

let chained = [ array; obj; printexc; random;scanf ]

let simples =
  List.map simple
    [ "Arg"; "ArrayLabels"; "Buffer"; "Bytes"; "BytesLabels";
      "Callback"; "CamlinternalFormat"; "CamlinternalFormatBasics";
      "Char"; "Complex"; "Digest"; "Filename"; "Format";
      "Gc"; "Genlex"; "Int32"; "Int64"; "Lazy"; "Lexing"; "List";
      "ListLabels"; "Marshal"; "NativeInt"; "Oo"; "Parsing"; "Printf"; "Queue";
      "Stack"; "Stream"; "String"; "StringLabels"; "Sys"; "Uchar"]

type 'p mk = ?nms:string -> ?mds:'a -> ?mts:'b -> 'c
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

let stdlabels =
  root "Stdlabels"
    ~mds:(List.map submodule ["Array"; "List"; "Bytes"; "String"])

let spacetime = root "Spacetime"
   ~mds:[submodule "Series"; submodule "Snapshot"]


let hms_base = [hashtbl; map; set]
let more_labels =
  root "MoreLabels"
    ~mds:(List.map ((|>) submodule) hms_base)

let hms = List.map ((|>) root) hms_base

let before v x = if version < v then x else []
let after v x = if version >= v then x else []

let bigarray ?nms () =
  root ?nms "Bigarray"
    ~mds:[
      submodule "Array0";
      submodule "Array1";
      submodule "Array2";
      submodule "Array3";
      submodule "Genarray";
    ]

let complex =
  weak :: hms @ more_labels :: stdlabels :: chained


let float = if version < (4,08) then simple "Float" else
    chain "Float" "Array"

let simple_stdlib = Dict.of_list @@
  before (4,08) [pervasives; simple "Sort"]
  @ after (4,03) [ephemeron]
  @ after (4,04) [spacetime]
  @ after (4,07) [bigarray ();float; simple "Seq"]
  @ after (4,08)
    (List.map simple ["Fun";"Bool";"Option";"Int";"Result";"LargeFile"; "Unit"])
  @ complex
  @ simples

let stdlib = Dict.of_list
    [Namespace {name="Stdlib"; modules=simple_stdlib }]


let num =
  top (simple ~nms:"stdlib/num")
    ["Arith_flags"; "Arith_status"; "Big_int"; "Int_misc"; "Nat"; "Num";
     "Ratio"]

let bigarray = Dict.of_list [bigarray ~nms:"stdlib/bigarray" ()]

let unix =
  top (fun x -> root ~nms:"stdlib/unix" x ~mds:[submodule "Largefile"] )
  [ "Unix"; "UnixLabels"]

let graphics =
  top (fun x -> root ~nms:"stdlib/graphics" x) [ "Graphics";"GraphicsX11"]

let threads =
  top (fun x -> root ~nms:"stdlib/threads" x)
    [ "Condition"; "Event"; "Mutex"; "Thread"; "ThreadUnix"]

let dynlink = Dict.of_list [root ~nms:"stdlib/dynlink" "Dynlink"]
