let bind x f = match x with
  | Some x -> f x
  | None -> None

let fmap f = function
  | Some x -> Some (f x)
  | None -> None

let either f default = function
  | Some x -> f x
  | None -> default

let default value = function
  | None -> value
  | Some x -> x

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

let rec list_join list = match list with
  | [] -> Some []
  | None :: _ -> None
  | Some x :: q ->
    q |> list_join >>| (fun q -> x :: q)

let rec list_map f list = match list with
  | [] -> Some []
  | x :: q ->
    f x >>= fun x ->
    list_map f q >>| fun q ->
    x :: q
