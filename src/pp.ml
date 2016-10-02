let fp = Format.fprintf

  let rec list ?(sep="; ") pp ppf =
    function
    | a :: ( _ :: _ as q ) -> fp ppf "%a%s@,%a" pp a sep (list ~sep pp) q
    | [a] -> fp ppf "%a" pp a
    | [] -> ()

  let blist pp ppf = fp ppf "[%a]" (list pp)
  let clist pp ppf = fp ppf "{%a}" (list pp)

let opt_list ?(pre="") ?(post="")  ?(sep="; ") pp ppf = function
  | [] -> ()
  | l -> fp ppf "%s@[<hv>%a@]%s" pre (list ~sep pp) l post


let string ppf = fp ppf "%s"

let decorate left right pp ppf x=
  fp ppf "%s%a%s" left pp x right

let std = Format.std_formatter
let err = Format.err_formatter
let p fmt = Format.printf fmt
let e fmt = Format.eprintf fmt

let pair ?(sep=",") pp1 pp2 ppf (x,y) = fp ppf "%a%s%a" pp1 x sep pp2 y
let snd pp ppf (_x,y) = fp ppf "%a" pp y
let fst pp ppf (x,_y) = fp ppf "%a" pp x

let opt ?(pre="") ?(post="") pp ppf = function
  | None -> ()
  | Some x -> fp ppf "%s" pre; pp ppf x; fp ppf "%s" post
