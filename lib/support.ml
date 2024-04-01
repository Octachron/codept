(* Xavier Leroy & Damien Doligez, INRIA Rocquencourt (c) 1996 *)

let is_dir_sep_win32 s i = let c = s.[i] in c = '/' || c = '\\' || c = ':'
let is_dir_sep_unix s i = s.[i] = '/'
let is_dir_sep = match Sys.os_type with
  | "Win32" -> is_dir_sep_win32
  | _ -> is_dir_sep_unix

let extension_len name =
  let rec check i0 i =
    if i < 0 || is_dir_sep name i then 0
    else if name.[i] = '.' then check i0 (i - 1)
    else String.length name - i0 in
  let rec search_dot i =
    if i < 0 || is_dir_sep name i then 0
    else if name.[i] = '.' then check i (i - 1)
    else search_dot (i - 1) in
  search_dot (String.length name - 1)

let extension name =
  let l = extension_len name in
  if l = 0 then "" else
  let l = l - 1 in
  String.sub name (String.length name - l) l

let remove_extension name =
  let l = extension_len name in
  if l = 0 then name else
  String.sub name 0 (String.length name - l)

let substring s start stop = String.sub s start (stop-start)

let split_on_char sep s =
  let rec split l last pos =
    if pos = 0 then
      if s.[pos] = sep then
        "" :: substring  s (pos+1) last :: l
      else
        substring s pos last :: l
    else if s.[pos] = sep then
      split (substring s (pos+1) last :: l) pos (pos-1)
    else
      split l last (pos-1) in
  let n = String.length s in
  split [] n (n-1)

type matcher =
  | Match of int
  | Reject of int

let split_on_matches sep s =
  let rec split on_match l last pos =
    if pos < 0 then
      (if on_match then ""::l else substring s 0 last :: l)
    else
      match sep s pos with
      | Match k -> split true (substring s (pos+1) last :: l) (1+pos-k) (pos-k)
      | Reject k -> split false l last (pos-k)
  in
  let n = String.length s in
  split false [] n (n-1)

let simple_matcher scheme s pos  =
  let ns = String.length scheme in
  if pos + 1 - ns < 0 then Reject ns
  else if substring s (pos+1-ns) (pos+1) = scheme then Match ns
  else Reject 1

let windows_and_unix s pos =
  if s.[pos] = '\\' || s.[pos] = '/' then Match 1 else Reject 1

let split_on_dirs s =
  let sep = Filename.dir_sep in
  if sep = "/" then
    split_on_char '/' s
  else if sep = "\\" then
    split_on_matches windows_and_unix s
  else split_on_matches (simple_matcher sep) s

let opt conv s = try Some(conv s) with Failure _ -> None

let filter_map f l =
   List.rev @@ List.fold_left (fun acc x ->
      match f x with
      | Some x -> x :: acc
      | None -> acc
    ) [] l

let is_upper = function
  | 'A' .. 'Z' -> true
  | _ -> false

let is_lower = function
  | 'a' .. 'z' -> true
  | _ -> false

let is_valid_module_char = function
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '_' | '\'' -> true
  | '-' -> true
    (* XXX(dinosaure): an example exists: [First-class-modules].
       [ocamlopt] can compile it but it emits an warning. *)
  | _ -> false

module Map = struct
  module type S = sig
     include Map.S
      val find_opt: key -> 'a t -> 'a option
  end
  module Make(X:Map.OrderedType) = struct
    include Map.Make(X)
    let find_opt k m = match find k m with
      | exception Not_found -> None
      | x -> Some x
  end
end
