(** A location within a file *)
type t =
  | Nowhere
  | Simple of { line:int; start:int; stop:int }
  | Multiline of { start: int * int; stop: int * int }

(** A data structure to add location data *)
type 'a ext = { loc:t; data:'a }

  let pp ppf = function
    | Nowhere -> ()
    | Simple {line;start;stop} -> Pp.fp ppf "l%d.%d−%d" line start stop
    | Multiline {start=(l1,c1); stop = (l2,c2) } ->
      Pp.fp ppf "l%d.%d−l%d.%d" l1 c1 l2 c2

  let create loc data = {loc; data}
  let nowhere data = { loc = Nowhere; data}

  let expand = function
    | Nowhere -> None
    | Simple {line;start;stop} -> Some ( (line,start), (line,stop) )
    | Multiline m -> Some(m.start, m.stop)

  let compress = function
    | Multiline {start; stop } when fst start = fst stop ->
        Simple { line = fst start; start = snd start; stop = snd stop }
    | (Simple _ | Nowhere | Multiline _) as m -> m


  let merge x y = compress @@
    match expand x, expand y with
    | None, None -> Nowhere
    | Some (start,stop) , None | None , Some(start,stop)
    | Some(start,_), Some(_,stop) ->
      Multiline {start;stop}

  let keep_one x y = match x, y with
    | Nowhere, x | x, Nowhere -> x
    | Simple _ as x, Multiline _
    | Multiline _, (Simple _ as x) -> x
    | Simple x as f, (Simple y as s) ->
      if x.start < y.start then f else s
    | Multiline x as f, (Multiline y as s) ->
      if x.start <= y.start then f else s

  let list l =
    List.fold_left (fun loc x -> merge loc x.loc) Nowhere l

  let fmap f x = { x with data = f x.data }



module Sch = struct
  open Schematic

  let raw_sch =
    Sum [ "Nowhere", Void; "Simple", [Int;Int;Int];
          "Multiline", [ [Int;Int]; [Int;Int] ] ]

  let t = let open Tuple in
    custom  raw_sch
      (function
        | Nowhere -> C E
        | Simple {line;start;stop} -> C(S (Z [line;start;stop]))
        | Multiline { start =l,c; stop = l',c' } -> C(S (S (Z [[l;c];[l';c']])))
      )
      (function
        | C E -> Nowhere
        | C S Z [line;start;stop] -> Simple {line;start;stop}
        | C S S Z [[l;c];[l';c']] -> Multiline {start=l,c; stop=l',c' }
        | _ -> .
      )

  let ext inner = let open Tuple in
    custom
      [ inner; t ] (fun r -> [r.data;r.loc]) (fun [data;loc] -> {data;loc})
end
