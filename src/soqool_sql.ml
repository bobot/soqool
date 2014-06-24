open Soqool_core


module Array = struct
  let get = Formula.get_term
  let set = Formula.set_term
end

let (&&) = Formula.Obj.fop2 "and"
let (||) = Formula.Obj.fop2 "or"
let not = Formula.Obj.fop1 "not"

module Return = struct

  module Array = struct
    let get = Return.get
  end

  let nil = Return.nil
  let (@) = Return.value
  let (@.) = Return.row

  let return = Return.return
end

let return1: ('table,_) Formula.from -> ('table row) Soqool_core.Return.t
  = fun r1 -> Return.(return (r1 @. nil) (fun x -> x))
let return2:
  ('table1,_) Formula.from -> ('table2,_) Formula.from ->
  ('table1 row * 'table2 row) Soqool_core.Return.t
  = fun r1 r2 -> Return.(return (r1  @. r2 @. nil)) (fun x y -> (x,y))

