  let fp = Format.fprintf
  let rec list ?(sep="; ") pp ppf =
    function
    | a :: ( _ :: _ as q ) -> fp ppf "%a%s%a" pp a sep (list ~sep pp) q
    | [a] -> fp ppf "%a" pp a
    | [] -> ()

  let blist pp ppf = fp ppf "[%a]" (list pp)
  let clist pp ppf = fp ppf "{%a}" (list pp)

let string ppf = fp ppf "%s"

let std = Format.std_formatter
let err = Format.err_formatter
let p fmt = Format.printf fmt
let e fmt = Format.eprintf fmt

let opt pp ppf = function
  | None -> ()
  | Some x -> pp ppf x
