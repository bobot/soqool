open Soqool_core.Formula.Obj

let (=)  t1 t2 = rel "=" t1 t2
let (<)  t1 t2 = rel "<" t1 t2
let (<=) t1 t2 = rel "<=" t1 t2
let (>)  t1 t2 = rel ">" t1 t2
let (>=) t1 t2 = rel ">=" t1 t2
let (<>) t1 t2 = rel "<>" t1 t2
