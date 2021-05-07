type t = { pkg:Pkg.t; loc:Loc.t }
let none = { pkg = Pkg.{ source= Unknown; file=Namespaced.make "" }; loc = Loc.Nowhere }

module Pp = struct

  let simple ppf {pkg;loc} = Pp.fp ppf "%a:%a" Pkg.pp pkg Loc.pp loc

  let tagged ppf l =
    Format_tags.(with_tag Loc) simple ppf l

  let opt ppf l =
    if l == none then ()
    else Pp.fp ppf "%a,@ " tagged l
end
