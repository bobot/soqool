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

  val exec:
    t ->
    ?expect : Postgresql.result_status list ->
    ?params : string array ->
    ?binary_params : bool array -> string ->
    (Postgresql.result -> 'a Deferred.t) ->
    ('a, exn) Core.Std.Result.t Deferred.t
    (**
       Direct execution of an sql command.
       Use it when you want to run command not accessible from the API.

       These case are interesting to know. Please report.

       In any case use with care, typing invariant can be broken doing so.
    *)

end

(** {2 Table, columns, rows}
    Columns and rows are statically associated to a unique table.
 *)

(** A table is associated to a unique type ['table] *)
type 'table table

(** It contains typed columns, ocaml type and postgresql type *)
type ('a,'table) column

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

  type 'a t
  (** 'a is the ocaml type *)

  module MkSexp (S:Sexpable.S): sig
    val t: S.t t
  end

  val from_sexp: ('a-> Sexp.t) -> (Sexp.t -> 'a) -> 'a t

  val id: 'table table -> 'table id t

  module Obj: sig
    val mk_with_string:
      Postgresql.ftype ->
      ('a -> string) ->
      (string -> 'a) -> 'a t
  end

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

module Formula: sig

  type ('table,'kind) from
      (** 'kind is [`UPDATED | `USED] *)

  type 'a term
  type formula
  type modify

  val get_term: ('table,_) from -> ('a,'table) column -> 'a term
  val set_term: ('table,[`UPDATED]) from ->
    ('a,'table) column -> 'a term -> modify

  module Obj: sig
    (** These functions must be used with the same caution than
        {!Obj.magic} *)

    val op2: string -> 'a term -> 'a term -> 'a term
    val op1: string -> 'a term -> 'a term
    (* val conv: string -> 'a term -> 'b term *)

    val fop2: string -> formula -> formula -> formula
    val fop1: string -> formula -> formula
    val rel: string -> 'a term -> 'a term -> formula

  end

end

module Return: sig

  type 'r t
  type 'a value

  val get: ('table,_) Formula.from -> ('a,'table) column -> 'a value

  type ('arg,'res) ft
  val nil: ('res,'res) ft
  val value : 'a value -> ('arg, 'res) ft -> ('a -> 'arg, 'res) ft
  val row: ('table,_) Formula.from -> ('arg, 'res) ft
    -> ('table row -> 'arg, 'res) ft

  val return: ('a,'b) ft -> 'a -> 'b t

end


module Params : sig
  type ('inner_arg,'outer_arg,'inner_res,'outer_res) t

  val nil: ('inner_res,'outer_res,'inner_res,'outer_res) t
  val cons:
    'a Ty.t ->
    ('inner_arg,'outer_arg,'inner_res,'outer_res) t ->
    ('p Formula.term -> 'inner_arg,'a -> 'outer_arg,'inner_res,'outer_res) t
end

(* (SQL.formula * 'r SQL.result, ('r list) exn_ret_defer) t *)


module From : sig
  type ('inner_arg,'inner_from) t

  val nil: ('inner_arg,'inner_arg) t
  val cons:
    'table table ->
    ('inner_arg,'inner_from) t ->
    ('inner_arg, ('table,[`USED]) Formula.from -> 'inner_from) t
end


type 'a select

val select:
  from:('inner_arg,'inner_from) From.t ->
  param:('inner_arg,'outer_arg,
         Formula.formula * 'r Return.t,
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
         Formula.formula * Formula.modify list,
         Int63.t exn_ret_defer) Params.t ->
  update:'table table ->
  (('table,[`UPDATED]) Formula.from -> 'inner_from) ->
  'outer_arg update

val exec_update:
  Connection.t ->
  'outer_arg update ->
  'outer_arg (** 'result is always of the form:
                 param1 -> param2 -> ... -> Int63.t exn_ret_defer
                 return the number of modified row *)

