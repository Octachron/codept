
let name = Format.sprintf "M%03d"
let filename = Format.sprintf "m%03d.mli"

let make n n_max =
  let f = open_out @@ filename n in
  let out = Format.formatter_of_out_channel f in
  let fp x = Format.fprintf out x in
  if n < n_max then
    for i = n_max downto 1 do
      fp "open %s " (name i);
      if i mod 6 = 5 then fp "\n"
    done;
  for i = 1 to n do
    fp "module %s: sig end\n%!" (name i)
  done;
  close_out f

let n = int_of_string @@ Sys.argv.(1)

let () =
  for i = 1 to n do
    make i n
  done;
