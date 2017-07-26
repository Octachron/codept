let lex src = Lexing.from_string src

let parse x =  Sparser.main Slex.main (lex x)

let wrap s = {Scheme.title = "test"; description=""; sch = s}

let counter = ref 0

let round_trip sch x =
  incr counter;
  let s = Format.asprintf "%a" (Scheme.json @@ wrap sch) x in
  let u = parse s in
  match Scheme.retype sch u with
  | Some y ->
    if x = y then
      Format.printf "@[%d) Success:@ %s@]@." !counter s
    else
      Format.printf "@[%d) Partial success:@ %s@]@." !counter s
  | None -> Format.printf "@[%d) Failure:@ %s@]@." !counter s


let round_trip' sch src =
  incr counter;
  let u = parse src in
  match Scheme.retype sch u with
  | None -> Format.printf "%d) Failure, typing error (%s)@." !counter src
  | Some x ->
    let s = Format.asprintf "%a" (Scheme.json @@ wrap sch) x in
    if s = src then
      Format.printf "%d) Success@." !counter
    else
      Format.printf "%d) Failure, expected:\n%s\ngot:\n%s@." !counter src s

open Scheme

module Y = Name(struct let s ="y" end)
module X = Name(struct let s="x" end)
module Z = Name(struct let s = "z" end)
;; round_trip String "𒆍𒀭𒊏𒆠"
;; round_trip Int 105
;; round_trip Bool false
;; round_trip Float 1.
;; round_trip (Array Int) [2;3;4;5]
;; round_trip [Int;Float] [1;1.]

;; round_trip [String; [Int;Bool]; Array (Array Int)]
  Tuple.["One"; [1;false]; L.[[1];[2];[3]]]

;; round_trip (Obj [Req,X.x,Int;Req,Y.x,Int]) [ X.x $= 1; Y.x $= 2 ]

;; round_trip (Array(Sum ["Int", Int;"String", String]))
  [C (Z 1); C (S (Z "1"))]

;; round_trip (Obj [Opt,X.x,Int;Opt,Y.x,String;Req,Z.x,Float])
  [ X.x $=? Some 1; skip Y.x; Z.x $= 2.]

;; round_trip' M2l.sch
  {|[[{
  "Minor" :
    {
    "access" :
      [[["Name"], { "Simple" : [7, 24, 30] }, "Normal"],
      [["Module"], { "Simple" : [6, 9, 17] }, "Normal"],
      [["Fault"], { "Simple" : [4, 61, 68] }, "Normal"]]
    }
  }, { "Multiline" : [[4, 0], [7, 53]] }],
[{
 "Bind_sig" :
   ["envt",
   {
   "Sig" :
     [[{
       "Minor" :
         {
         "access" :
           [[["Summary"], { "Simple" : [16, 18, 27] }, "Normal"],
           [["Paths", "Simple"], { "Simple" : [13, 19, 33] }, "Normal"],
           [["Paths", "S"], { "Simple" : [18, 32, 41] }, "Normal"],
           [["Namespaced"], { "Simple" : [17, 44, 56] }, "Normal"],
           [["Module"], { "Simple" : [14, 33, 45] }, "Normal"],
           [["Format"], { "Simple" : [21, 10, 26] }, "Normal"],
           [["Deps", "Edge"], { "Simple" : [14, 18, 29] }, "Normal"]]
         }
       }, { "Multiline" : [[13, 2], [21, 39]] }]]
   }]
 }, { "Multiline" : [[10, 0], [22, 3]] }],
[{
 "Bind_sig" :
   ["with_deps",
   {
   "Sig" :
     [[{
       "Minor" :
         { "access" : [[["Deps"], { "Simple" : [26, 17, 23] }, "Normal"]] }
       }, { "Simple" : [26, 2, 23] }]]
   }]
 }, { "Multiline" : [[24, 0], [28, 3]] }],
[{
 "Bind_sig" :
   ["envt_with_deps",
   {
   "Sig" :
     [[{ "SigInclude" : { "With" : [{ "Ident" : { "A" : "envt" } }, []] } },
      { "Simple" : [32, 2, 31] }],
     [{
      "SigInclude" : { "With" : [{ "Ident" : { "A" : "with_deps" } }, []] }
      }, { "Simple" : [33, 2, 36] }]]
   }]
 }, { "Multiline" : [[30, 0], [34, 3]] }],
[{
 "Bind_sig" :
   ["param",
   {
   "Sig" :
     [[{
       "Minor" :
         {
         "access" :
           [[["Fault", "Policy"], { "Simple" : [39, 14, 28] }, "Normal"]]
         }
       }, { "Simple" : [39, 2, 28] }]]
   }]
 }, { "Multiline" : [[37, 0], [43, 3]] }],
[{
 "Bind_sig" :
   ["s",
   {
   "Sig" :
     [[{
       "Minor" :
         {
         "access" :
           [[["Paths", "P"], { "Simple" : [49, 12, 21] }, "Normal"],
           [["Module", "Sig"], { "Simple" : [49, 50, 62] }, "Normal"],
           [["M2l"], { "Simple" : [49, 33, 38] }, "Normal"]]
         }
       }, { "Simple" : [49, 2, 77] }]]
   }]
 }, { "Multiline" : [[46, 0], [50, 3]] }],
[{
 "Bind" :
   ["Make",
   {
   "Constraint" :
     ["Abstract",
     {
     "Fun" :
       [{ "Some" : ["Envt", { "Ident" : { "A" : "envt" } }] },
       {
       "Fun" :
         [{ "Some" : ["Param", { "Ident" : { "A" : "param" } }] },
         { "With" : [{ "Ident" : { "A" : "s" } }, []] }]
       }]
     }]
   }]
 }, { "Multiline" : [[53, 0], [54, 69]] }]]|}
