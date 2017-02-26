let extension_pos name =
    let n = String.length name in
  let r = try String.rindex name '.' with Not_found -> n-1 in
    if r = 0 then
      try String.rindex name '.' with Not_found -> n-1
    else r

let extension name =
  let n = String.length name in
  let r = extension_pos name in
  String.sub name (r+1) (n-r-1)

let remove_extension name =
  let r = extension_pos name in
  String.sub name 0 r

let split_on_char sep s =
  let sub start stop =
    String.sub s start (stop-start) in
  let rec split l last pos =
    if pos = 0 then
      if s.[pos] = sep then
        "" :: sub (pos+1) last :: l
      else
        sub pos last :: l
    else if s.[pos] = sep then
      split (sub (pos+1) last :: l) pos (pos-1)
    else
      split l last (pos-1) in
  let n = String.length s in
  split [] n (n-1)
