open Core.Std
open Soqool_core

module type Num = sig
  open Formula

  type t

  val t: t Ty.t

  val (=): t term -> t term -> formula
  val (<): t term -> t term -> formula
  val (>): t term -> t term -> formula
  val (<=): t term -> t term -> formula
  val (>=): t term -> t term -> formula
  val (<>): t term -> t term -> formula

  val (+): t term -> t term -> t term
  val (-): t term -> t term -> t term
  val ( * ): t term -> t term -> t term
  val (mod): t term -> t term -> t term
  val (/): t term -> t term -> t term
  val ( ** ): t term -> t term -> t term

  val (lor):  t term -> t term -> t term
  val (land): t term -> t term -> t term
  val (lxor): t term -> t term -> t term
  val lnot: t term -> t term
  val (lsl): t term -> t term -> t term
  val (lsr): t term -> t term -> t term

end


module Gen(X:sig type t
             val ftype : Postgresql.ftype
             val to_string: t -> string
             val of_string: string -> t
           end): Num with type t = X.t =
struct

  type t = X.t

  let t = Ty.Obj.mk_with_string X.ftype X.to_string X.of_string

  open Formula.Obj

  let (=)  t1 t2 = rel "=" t1 t2
  let (<)  t1 t2 = rel "<" t1 t2
  let (<=) t1 t2 = rel "<=" t1 t2
  let (>)  t1 t2 = rel ">" t1 t2
  let (>=) t1 t2 = rel ">=" t1 t2
  let (<>) t1 t2 = rel "<>" t1 t2

  let (+) t1 t2 = op2 "+" t1 t2
  let (-) t1 t2 = op2 "-" t1 t2
  let ( * ) t1 t2 = op2 "*" t1 t2
  let ( ** ) t1 t2 = op2 "^" t1 t2
  let (mod) t1 t2 = op2 "%" t1 t2
  let (/) t1 t2 = op2 "/" t1 t2

  let (land) t1 t2 = op2 "&" t1 t2
  let (lor) t1 t2 = op2 "|" t1 t2
  let (lxor) t1 t2 = op2 "#" t1 t2
  let lnot t1 = op1 "|" t1
  let (lsl) t1 t2 = op2 "<<" t1 t2
  let (lsr) t1 t2 = op2 ">>" t1 t2
end


module Int32 = Gen(struct
    include Int32
    let ftype = Postgresql.INT4
  end
  )

module Int64 = Gen(struct
    include Int64
    let ftype = Postgresql.INT8
  end
  )

module Int63 = Gen(struct
    include Int63
    let ftype = Postgresql.INT8
  end
  )

module Float = Gen(struct
    include Float
    let ftype = Postgresql.FLOAT8
  end
  )

module String = struct

  type t = String.t
  let t = Ty.Obj.mk_with_string
      Postgresql.TEXT
      (fun x -> x) (** no \000 allowed! *)
      (fun x -> x)

  let (^) = Formula.Obj.op2 "||"
end

(** double precision *)

module Ty = struct
  let int32 = Int32.t
  let int64 = Int64.t
  let int63 = Int63.t
  let string = String.t
  let float = Float.t
end
