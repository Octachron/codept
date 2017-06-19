let bind x f = match x with
  | Some x -> f x
  | None -> None

let fmap f = function
  | Some x -> Some (f x)
  | None -> None


let either f default = function
  | Some x -> f x
  | None -> default


let iter f = either f ()

let default value = function
  | None -> value
  | Some x -> x

let lazy_default suspended = function
  | None -> Lazy.force suspended
  | Some x -> x

let (|||) x y = match x with
  | None -> Lazy.force y
  | Some _ as x -> x

let join  = function
  | Some x -> x
  | None -> None

let (>>|) x f= fmap f x
let (>>=) = bind

let (>>) x y = match x with
  | None -> None
  | Some _ -> y

let (><) opt def = default def opt

let (&&) x y = match x,y with
  | Some x, Some y -> Some (x,y)
  | _ -> None

let mcons list x =
  match x with Some x -> x :: list | None -> list

module List' = struct
  let filter l = List.fold_left mcons [] l
  let rec join list = match list with
    | [] -> Some []
    | None :: _ -> None
    | Some x :: q ->
      q |> join >>| (fun q -> x :: q)

  let rec map f list = match list with
    | [] -> Some []
    | x :: q ->
      f x >>= fun x ->
      map f q >>| fun q ->
      x :: q

end
