type ('a,'b) t = ('a,'b) result

let is_ok = function Ok _ -> true | Error _ -> false

let all_done undone l =
  if List.for_all is_ok l then
    Ok (List.map (function Ok x -> x | _ -> assert false ) l)
  else
    Error (List.map
              (function Ok d -> undone d
                      | Error h -> h ) l)

let fmap f g = function
  | Error x -> Error (f x)
  | Ok r -> Ok (g r)

let fmap_ok f = function
  | Error _ as h  -> h
  | Ok r -> Ok (f r)

let fmap_error f = function
  | Error h  -> Error (f h)
  | Ok _ as r -> r
