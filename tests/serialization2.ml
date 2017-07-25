let lex src = Lexing.from_string src

let parse x =  Sparser.main Slex.main (lex x)

let wrap s = {Scheme.title = "test"; description=""; sch = s}

let counter = ref 0
let round_trip sch x =
  incr counter;
  let s = Format.asprintf "%a" (Scheme.json @@ wrap sch) x in
  let u = parse s in
  match Scheme.retype sch u with
  | Some _ -> Format.printf "@[%d: Success:@ %s@]@." !counter s
  | None -> Format.printf "@[%d: Failure:@ %s@]@." !counter s

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
