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

(** integral type: int32 *)
module Int32: Num with type t = Int32.t

(** integral type: int64 *)
module Int64: Num with type t = Int64.t

(** integral type: int63 but the sql type is a 8bit long
    arithmetic modulo, lsr are not respected
*)
module Int63: Num with type t = Int63.t

module Float: Num with type t = Float.t

module String: sig
  open Formula

  type t = String.t
  val t: t Ty.t

  val (^): t term -> t term -> t term

end

(** double precision *)

module Ty : sig
  val int32: Int32.t Soqool_core.Ty.t
  val int64: Int64.t Soqool_core.Ty.t
  val int63: Int63.t Soqool_core.Ty.t
  val string: string Soqool_core.Ty.t

  (** double precision *)
  val float: float Soqool_core.Ty.t
end
