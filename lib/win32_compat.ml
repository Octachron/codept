let dos2unix s =
  let len = String.length s in
  let b = Buffer.create len in
  for i = 0 to len - 1 do
    match s.[i] with '\r' -> () | c -> Buffer.add_char b c
  done;
  Buffer.contents b
