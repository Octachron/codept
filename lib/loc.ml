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

  let list l =
    List.fold_left (fun loc x -> merge loc x.loc) Nowhere l

  let fmap f x = { x with data = f x.data }


module Sexp = struct
  open Sexp
    let simple = C2.C { name = "Simple";
                        proj = (function Simple s -> Some(s.line, s.start, s.stop)
                                       | _ -> None ) ;
                        inj = (fun (line,start,stop) -> Simple {line;start;stop} );
                        impl = triple' int int int;
                      }
    let multiline = C2.C
        { name = "Multiline";
          proj = (function Multiline {start;stop} ->
              Some(fst start, snd start, fst stop, snd stop )
                         | _ -> None );
          inj = (fun (l,c,l2,c2) -> Multiline { start = l, c; stop = l2, c2 });
          impl = tetra' int int int int
        }

    let empty_set =
      let parse = function
        | Keyed_list ("∅", []) -> Some ()
        | _ -> None
      and embed _ = Keyed_list ("∅", [] ) in
      {parse;embed;kind= One_and_many }

    let nowhere = C2.C
        { name= "Nowhere";
          proj = (function Nowhere -> Some () | _ -> None );
          inj = (fun _ -> Nowhere);
          impl = empty_set;
        }


  let t =
    C2.sum simple [simple;multiline;nowhere]

  let ext impl =
    Sexp.convr (Sexp.pair impl t)
      (fun (data,loc) -> {data;loc})
      (fun r -> r.data, r.loc)


end

module Sch = struct
  open Scheme

  let raw_sch =
    Sum [ Void; [Int;Int;Int]; [ [Int;Int]; [Int;Int] ] ]

  let t = let open Tuple in
    custom "Loc.t" raw_sch
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

  let ext name inner = let open Tuple in
    custom ("Loc.ext."^name)
      [ inner; t ] (fun r -> [r.data;r.loc]) (fun [data;loc] -> {data;loc})
end
