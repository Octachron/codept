type kind = Interface | Implementation | Signature
type info = { format: Read.format; kind : kind }

let classic {format;kind}: Read.kind option = match kind with
  | Interface -> Some { format; kind = M2l.Signature }
  | Implementation -> Some { format; kind = M2l.Structure }
  | Signature -> None

let ml = { format=Src; kind = Implementation }
let mli = { format=Src; kind = Interface }
