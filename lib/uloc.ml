type t = { pkg:Paths.Pkg.t; loc:Loc.t }
let none = { pkg = Paths.Pkg.{ source= Unknown; file=[] }; loc = Loc.Nowhere }

module Pp = struct

  let simple ppf {pkg;loc} = Pp.fp ppf "%a:%a" Paths.Pkg.pp pkg Loc.pp loc

  let tagged ppf l =
    Format_tags.(with_tag Loc) simple ppf l

  let opt ppf l =
    if l == none then ()
    else Pp.fp ppf "%a,@ " tagged l
end
