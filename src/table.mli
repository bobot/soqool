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

(** Table API *)

open Core.Std
open Async.Std

(** {2 Connection} *)
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
  (** create a database connection *)


  val close: t -> (unit,exn) Result.t Deferred.t
  (** close a database connection *)

end

(** {2 Table, columns, rows}
    Columns and rows are statically associated to a unique table.
 *)

(** A table is associated to a unique type ['table].
    The row of the table represent a type 'r *)
type 'table table

(** It contains typed columns, ocaml type and postgresql type *)
type ('a,'p,'table) column

(** The result of a query: a row of the table ['table] *)
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

  type ('a,'p) t
  (** 'a is the ocaml type and 'p is the postgresql type *)

  module MkSexp (S:Sexpable.S): sig
    val t: (S.t,[`SEXP]) t
  end

  val from_sexp: ('a-> Sexp.t) -> (Sexp.t -> 'a) -> ('a,[`SEXP]) t

  (** integral type *)
  val int: (int,[`INT]) t
  val int32: (Int32.t,[`INT]) t
  val int64: (Int64.t,[`INT]) t
  val int63: (Int63.t,[`INT]) t
  val string: (string,[`TEXT]) t

  (** double precision *)
  val float: (float,[`FLOAT]) t

  val id: 'table table -> ('table id,[`ID]) t

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
    ('a,'p) Ty.t ->
    ('add, 'a -> 'res) uc ->
    ('a,'p,t) column * ('add, 'res) uc

  val close:
    ('add, (t id) exn_ret_defer) uc ->
    t table * 'add adder * (t id,[`ID],t) column
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

val get: ('a,_,'table) column -> 'table row -> 'a

module SQL: sig

  type ('table,'kind) from
  type 'a term
  type formula
  type modify

  module Array: sig
    val get: ('table,_) from -> ('a,'p,'table) column -> 'p term
    val set:
      ('table,[`UPDATED]) from ->
      ('a,'p,'table) column -> 'p term -> modify
  end

  (** comparison operator are implemented for all relevant datatype
      (I don't know what relevant means)
      Warning it is the comparison operator of posgresql.
      It must be the same than ocaml for predefined type in Ty.
      But certainly not for the one created with Ty.from_sexp
  *)
  val (=): 'p term -> 'p term -> formula
  val (<): 'p term -> 'p term -> formula
  val (>): 'p term -> 'p term -> formula
  val (<=): 'p term -> 'p term -> formula
  val (>=): 'p term -> 'p term -> formula
  val (<>): 'p term -> 'p term -> formula

  type 'a num = [< `INT | `FLOAT] as 'a
  val (+): 'a num term -> 'a num term -> 'a num term
  val (-): 'a num term -> 'a num term -> 'a num term
  val ( * ): 'a num term -> 'a num term -> 'a num term
  val (mod): 'a num term -> 'a num term -> 'a num term
  val (/): 'a num term -> 'a num term -> 'a num term
  val ( ** ): 'a num term -> 'a num term -> 'a num term

  val (lor):  [`INT] term -> [`INT] term -> [`INT] term
  val (land): [`INT] term -> [`INT] term -> [`INT] term
  val (lxor): [`INT] term -> [`INT] term -> [`INT] term
  val lnot: [`INT] term -> [`INT] term
  val (lsl): [`INT] term -> [`INT] term -> [`INT] term
  val (lsr): [`INT] term -> [`INT] term -> [`INT] term


  val (&&): formula -> formula -> formula
  val (||): formula -> formula -> formula
  val not: formula -> formula

  module Build: sig
    val rel: string -> 'a term -> 'a term -> formula
    val op2: string -> 'a term -> 'a term -> 'a term
    val op1: string -> 'a term -> 'a term
  end

  module Return: sig

    type 'r t
    type 'a value

    module Array: sig
      val get: ('table,_) from -> ('a,'p,'table) column -> 'a value
    end

    type ('arg,'res) ft
    val nil: ('res,'res) ft
    val (@) : 'a value -> ('arg, 'res) ft -> ('a -> 'arg, 'res) ft
    val (@.): ('table,_) from -> ('arg, 'res) ft
      -> ('table row -> 'arg, 'res) ft

    val return: 'a -> ('a,'b) ft -> 'b t

  end

  val return1: ('table,_) from -> ('table row) Return.t
  val return2:
    ('table1,_) from -> ('table2,_) from ->
    ('table1 row * 'table2 row) Return.t

end

module Params : sig
  type ('inner_arg,'outer_arg,'inner_res,'outer_res) t

  val nil: ('inner_res,'outer_res,'inner_res,'outer_res) t
  val (@):
    ('a,'p) Ty.t ->
    ('inner_arg,'outer_arg,'inner_res,'outer_res) t ->
    ('p SQL.term -> 'inner_arg,'a -> 'outer_arg,'inner_res,'outer_res) t
end

    (* (SQL.formula * 'r SQL.result, ('r list) exn_ret_defer) t *)


module From : sig
  type ('inner_arg,'inner_from) t

  val nil: ('inner_arg,'inner_arg) t
  val (@):
    'table table ->
    ('inner_arg,'inner_from) t ->
    ('inner_arg, ('table,[`USED]) SQL.from -> 'inner_from) t
end


type 'a select

val select:
  from:('inner_arg,'inner_from) From.t ->
  param:('inner_arg,'outer_arg,
         SQL.formula * 'r SQL.Return.t,
         ('r list) exn_ret_defer) Params.t ->
  'inner_from ->
  'outer_arg select

val exec_select:
  Connection.t ->
  'outer_arg select ->
  'outer_arg (** 'result is always of the form:
                 param1 -> param2 -> ... -> ('r list) exn_ret_defer *)

type 'a update

val update:
  from:('inner_arg,'inner_from) From.t ->
  param:('inner_arg,'outer_arg,
         SQL.formula * SQL.modify list,
         Int63.t exn_ret_defer) Params.t ->
  update:'table table ->
  (('table,[`UPDATED]) SQL.from -> 'inner_from) ->
  'outer_arg update

val exec_update:
  Connection.t ->
  'outer_arg update ->
  'outer_arg (** 'result is always of the form:
                 param1 -> param2 -> ... -> Int63.t exn_ret_defer
                 return the number of modified row *)

