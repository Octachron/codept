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

module Ok = struct
  let fmap f = function
    | Error _ as h  -> h
    | Ok r -> Ok (f r)
  let (>>|) x f = fmap f x

  let bind f = function
    | Error _ as h  -> h
    | Ok r -> f r
  let (>>=) x f = bind f x
end

module Error = struct
  let fmap f = function
    | Error h  -> Error (f h)
    | Ok _ as r -> r
  let (>>|) x f = fmap f x

  let bind f = function
    | Error h  -> f h
    | Ok _ as r -> r

  let (>>=) x f = bind f x

end
