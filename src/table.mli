(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2013                                                    *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open Core.Std
open Async.Std

(** {2 connection} *)
module Connection: sig
  type t

  val create:
    ?verbose:bool ->         (** default: false*)
    ?host : string ->        (** Default: none *)
    ?hostaddr : string ->    (** Default: none *)
    ?port : string  ->       (** Default: none *)
    ?dbname : string ->      (** Default: none *)
    ?user : string ->        (** Default: none *)
    ?password : string ->    (** Default: none *)
    ?options : string ->     (** Default: none *)
    ?tty : string ->         (** Default: none *)
    ?requiressl : string ->  (** Default: none *)
    ?conninfo : string ->    (** Default: none *)
    unit -> (t,exn) Result.t Deferred.t

  val close: t -> (unit,exn) Result.t Deferred.t

end

(** {2 Table, Column} *)

(** A table is associated to a unique type ['table].
    The row of the table represent a type 'r *)
type 'table table

(** It contains typed columns *)
type ('a,'table) column

(** The result of a query: a raw of the table ['table] *)
type 'table row

(** A row can be added using *)
type 'add adder


(** The unique identifier of a row (SERIAL PRIMARY KEY of the table) *)
module Id: sig
  type 'table t
  val hash: 'table t -> int
  val compare: 'table t -> 'table t -> int
  val to_string: 'table t -> string
end
type 'table id = 'table Id.t

type col_constr = UNIQUE

type 'a exn_ret_defer = ('a, exn) Result.t Deferred.t

(** {2 column type} *)

module Ty: sig

  type 'a t

  module MkSexp (S:Sexpable.S): sig
    val t: S.t t
  end

  val from_sexp: ('a-> Sexp.t) -> (Sexp.t -> 'a) -> 'a t

  val int: int t
  val int32: Int32.t t
  val int64: Int64.t t
  val int63: Int63.t t
  val string: string t
  val float: float t

  val id: 'table table -> 'table id t

end

module MkTable(X:sig val name:string val version:int end): sig
  type t

  type ('add,'res) uc (** under connstruction *)
    (** after close the shape of ['add] is
        [ty1 -> ty2 -> ... ->  (t id) exn_ret_defer].
        ['res] is just an internal way for adding applications at the end
    *)

  val uc: ('res, 'res) uc

  val add_column:
    ?constraints: col_constr list -> (* default: [] *)
    name:string ->
    'a Ty.t ->
    ('add, 'a -> 'res) uc ->
    ('a,t) column * ('add, 'res) uc

  val close:
    ('add, (t id) exn_ret_defer) uc ->
    t table * 'add adder * (t id,t) column
  (** No more column can be added  after calling this function.
      Return the table and the id column of the table *)
end

val create_table:
  Connection.t ->
  ?if_not_exists:bool ->   (** default true *)
  _ table ->
  unit exn_ret_defer

val drop_table:
  Connection.t ->
  ?if_exists:bool ->
  _ table ->
  unit exn_ret_defer

val add_row:
  Connection.t ->
  'add adder ->
  'add (** end by ('table id) exn_ret_defer *)

val get: ('a,'table) column -> 'table row -> 'a

module SQL: sig

  type 'table from
  type 'a term
  type formula

  module Array: sig
    val get: 'table from -> ('a,'table) column -> 'a term
  end

  (** comparison operator are implemented for all relevant datatype
      (I don't know what relevant means)
      Warning it is the comparison operator of posgresql.
      It must be the same than ocaml for predefined type in Ty.
      But certainly not for the one created with Ty.from_sexp
  *)
  val (=): 'a term -> 'a term -> formula
  val (<): 'a term -> 'a term -> formula
  val (>): 'a term -> 'a term -> formula
  val (<=): 'a term -> 'a term -> formula
  val (>=): 'a term -> 'a term -> formula
  val (<>): 'a term -> 'a term -> formula


  val (&&): formula -> formula -> formula
  val (||): formula -> formula -> formula
  val not: formula -> formula

  type 'r result

  val result1: 'table from -> ('table row) result
  val result2:
    'table1 from -> 'table2 from ->
    ('table1 row * 'table2 row) result

end

module Params : sig
  type ('params,'result) t

  val e:
    (SQL.formula * 'r SQL.result, ('r list) exn_ret_defer) t
  val (@):
    'a Ty.t -> ('params,'result) t ->
    ('a SQL.term -> 'params,'a -> 'result) t
end

module From : sig
  type ('params,'formula) t

  val e: ('params,'params) t
  val (@):
    'table table -> ('params,'formula) t ->
    ('params, 'table SQL.from -> 'formula) t
end


type 'a select

val select:
  from:('params,'formula) From.t ->
  param:('params,'result) Params.t ->
  'formula ->
  'result select

val exec_select:
  Connection.t ->
  'result select ->
  'result (** 'result is always of the form:
              param1 -> param2 -> ... -> ('r list) exn_ret_defer *)
