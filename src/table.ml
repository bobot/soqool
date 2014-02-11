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


(** register Postgres into sexp lib and so inside Printexc.register_printer *)
let () =
  StdLabels.List.iter
    ~f:(fun (exc, handler) ->
      Sexplib.Conv.Exn_converter.add_auto ~finalise:false exc handler)
    [
      (Postgresql.Error Postgresql.Binary,
       function
       | Postgresql.Error error ->
         let str_err = Postgresql.string_of_error error in
         Sexp.List [
           Sexp.Atom "Postgresql.Error";
           Sexp.Atom str_err;
         ]
       | _ -> assert false (** absurd: by add_auto invariant *)
      );
      (Postgresql.Oid(0),
       function
       | Postgresql.Oid oid ->
         Sexp.List [
           Sexp.Atom "Postgresql.Error";
           Sexp.Atom
             "conversion from an oid to an ftype encountered an unknown [oid]";
           Int.sexp_of_t oid;
         ]
       | _ -> assert false (** absurd: by add_auto invariant *)
      )
    ]

module Connection : sig
  type t

  val create:
    ?verbose:bool ->
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

  val exec:
    t ->
    ?expect : Postgresql.result_status list ->
    ?params : string array ->
    ?binary_params : bool array -> string ->
    (Postgresql.result -> 'a Async_core.Deferred.t) ->
    ('a, exn) Core.Std.Result.t Async_core.Deferred.t

end
 = struct
  type t = {
    db: Postgresql.connection;
    (** libpq need not all the communication to be done with the same thread
        but the call can't be interleaved *)
    thread: In_thread.Helper_thread.t;
    verbose:bool;
  }


  let in_thread thread call handle =
    try_with ~extract_exn:true ~run:`Now (fun () ->
        In_thread.run ~thread ~name:"Table.db.connection" call
        >>= handle)

  let create
    ?(verbose=false)
      ?host ?hostaddr ?port ?dbname ?user ?password ?options ?tty
      ?requiressl ?conninfo
      () =
    let thread =
      Or_error.ok_exn (In_thread.Helper_thread.create
                         ~name:(Printf.sprintf "postgresql_%s"
                                  (Option.value ~default:"" dbname)) ()) in
    in_thread thread
      (fun () -> new Postgresql.connection
        ?host ?hostaddr ?port ?dbname ?user ?password ?options ?tty
        ?requiressl ?conninfo
        ())
      (fun db ->
         if not verbose then db#set_notice_processor (fun _ -> ());
         Deferred.return { db; thread; verbose })


  let in_thread conn call handle =
    try_with ~extract_exn:true ~run:`Now (fun () ->
        In_thread.run
          ~thread:conn.thread
          ~name:"Table.db.connection" (fun () -> call conn.db)
        >>= handle)

  let exec conn ?expect ?params ?binary_params request f =
    if conn.verbose then begin
      eprintf "Scheduled: exec: %s\n" request;
      Option.iter ~f:(Array.iteri
                        ~f:(fun i s -> eprintf "           $%i:%s\n" (i+1) s))
                        params
    end;
    in_thread conn (fun db ->
        db#exec ?expect ?params ?binary_params request)
      f



  let close t =
    in_thread t
      (fun db -> db#finish)
      (fun () -> Deferred.unit)

end

module Id :  sig
  type 'table t = Int63.t
  val hash: 'table t -> int
  val compare: 'table t -> 'table t -> int
  val to_string: 'table t -> string
end = struct
  type 'table t = Int63.t
  let hash = Int63.hash
  let compare = Int63.compare
  let to_string = Int63.to_string
end
type 'rt id = 'rt Id.t (** serial type *)

type col_constr = UNIQUE

type 'a exn_ret_defer = ('a, exn) Result.t Deferred.t

type 'rt table = {
  name:string;
  version:int;
  columns: 'rt columns;
}

and 'add adder = Connection.t -> 'add

and 'rt columns =
  | Nil
  | Cons: ('a,'p,'rt) column * 'rt columns -> 'rt columns

and ('a,'p,'rt) column = {
  ty  : ('a,'p) ty;
  name: string;
  index: int;
  constraints: col_constr list;
}

and 'rt row = {
  result: Postgresql.result;
  index: int;  (** tuple index *)
  offset: int; (** first field *)
}

and 'a kind =
  | Data: Postgresql.ftype -> 'a kind
  | Foreign_key: 'table table -> Int63.t kind (** or primary key *)

and ('a,'p) ty = {
  kind: 'a kind;
  sql_of_t: 'a -> string;
  t_of_sql: string -> 'a;
}

let rec nbcolumns acc = function
  | Nil -> acc
  | Cons(_,l) -> nbcolumns (acc+1) l

module Ty = struct

  type ('a,'p) t = ('a,'p) ty

  (** TODO binary format *)

  let from_sexp sexp_of_t t_of_sexp = {
    kind = Data Postgresql.TEXT;
    sql_of_t = (fun x -> Sexp.to_string (sexp_of_t x));
    t_of_sql = (fun x -> t_of_sexp (Sexp.of_string x));
  }

  module MkSexp (S:Sexpable.S)= struct
    let t = from_sexp S.sexp_of_t S.t_of_sexp
  end

  let int32 = { kind = Data Postgresql.INT4;
                           sql_of_t = Int32.to_string;
                           t_of_sql = Int32.of_string }
  let int64 = { kind = Data Postgresql.INT8;
                           sql_of_t = Int64.to_string;
                           t_of_sql = Int64.of_string }
  let int63 = { kind = Data Postgresql.INT8;
                           sql_of_t = Int63.to_string;
                           t_of_sql = Int63.of_string }
  let int = { kind = Data Postgresql.INT8;
                     sql_of_t = Int.to_string;
                     t_of_sql = Int.of_string }
  let string = { kind = Data Postgresql.TEXT;
                           sql_of_t = (fun x -> x);
                           t_of_sql = (fun x -> x)
                         }
  let float = { kind = Data Postgresql.FLOAT8;
                         sql_of_t = Float.to_string;
                         t_of_sql = Float.of_string;
                       }

  let id table = { kind = Foreign_key table;
                   sql_of_t = Int63.to_string;
                   t_of_sql = Int63.of_string;
                 }

end

exception Already_closed_table of string * int with sexp

let table_name table = table.name ^ "_" ^ (Int.to_string table.version)

module MkTable(X:sig val name:string val version:int end) = struct
  type t

  type ('add,'res) uc =
    | Nil: ('add,'add) uc
    | Cons: ('a,'p) ty * ('add,'a -> 'res)  uc -> ('add,'res) uc

  let uc = Nil

  let index = ref 0 (** 0 for primary key *)
  let columns = ref (Nil : 'a columns)

  let add_column ?(constraints=[]) ~name ty uc =
    if !index = -2 then raise (Already_closed_table(X.name,X.version));
    incr index;
    let col = {ty; name = Printf.sprintf "%s_%i" name !index;
               index = !index; constraints} in
    columns := Cons(col,!columns);
    col, Cons(ty,uc)

  let rec rev_columns acc : 'a columns -> 'a columns = function
    | Nil -> acc
    | Cons(col,l) -> rev_columns (Cons(col,acc)) l

  let sql_insert table columns =
    let rec aux b : 'a columns -> unit = function
      | Nil -> ()
      | Cons (col,Nil) ->
        Printf.bprintf b "$%i" col.index
      | Cons (col,l) ->
        Printf.bprintf b "$%i," col.index;
        aux b l in
    let b  = Buffer.create 70 in
    Printf.bprintf b "INSERT INTO %s VALUES (DEFAULT,%a) RETURNING id;"
      (table_name table) aux columns;
    Buffer.contents b

  let close: type add.
    (add, (t id) exn_ret_defer) uc ->
    t table * add adder * (t id,[`ID],t) column =
    fun  uc ->
      let columns = rev_columns Nil !columns in
      let len = !index in
      let rec id = {
        ty   = { kind = Foreign_key table;
                 sql_of_t = Int63.to_string;
                 t_of_sql = Int63.of_string;
               };
        name = "id"; index = 0; constraints = []
      }
      and table =  {
        name = X.name;
        version = X.version;
        columns = Cons(id, columns);
      } in
      let rec gen_adder: type add res.
        (add,res) uc -> int -> (** index *)
        (Connection.t -> string Array.t -> res) ->
        (Connection.t -> string Array.t -> add)
        = fun uc i exec ->
        match uc with
        | Nil -> exec
        | Cons(ty,l) ->
          let exec conn array x =
            array.(i) <- ty.sql_of_t x;
            exec conn array
          in
          gen_adder l (i-1) exec
      in
      let request = sql_insert table columns in
      let exec conn array =
        Connection.exec
          conn
          ~expect:[Postgresql.Tuples_ok]
          ~params:array
          request
          (fun result ->
             assert (result#ntuples = 1);
             assert (result#nfields = 1);
             Deferred.return (Int63.of_string (result#getvalue 0 0))
            )
      in
      let exec = gen_adder uc (len-1) exec in
      let exec conn = exec conn (Array.create ~len "") in
      index := -2;
      table, exec, id
end

let create_table conn ?(if_not_exists=true) table =
  let rec sql_columns b : 'table columns -> unit = function
    | Nil -> ()
    | Cons(col,l) ->
      Buffer.add_string b col.name;
      Buffer.add_char b ' ';
      begin
        match col.ty.kind with
        | Data ftype ->
          Buffer.add_string b (Postgresql.string_of_ftype ftype)
        | Foreign_key table' when phys_same table table' -> (** primary key *)
          Buffer.add_string b "SERIAL PRIMARY KEY"
        | Foreign_key table' ->
          Printf.bprintf b "int4 REFERENCES %s(id)" (table_name table')
      end;
      if not (phys_equal l Nil) then Buffer.add_string b ",\n";
      sql_columns b l
  in
  let b = Buffer.create 100 in
  Printf.bprintf b "CREATE TABLE%s%s (%a);"
    (if if_not_exists then " IF NOT EXISTS " else "")
    (table_name table)
    sql_columns table.columns;
  let request = Buffer.contents b in
  Connection.exec conn ~expect:[Postgresql.Command_ok] request
    (fun _ -> Deferred.unit)

let drop_table conn ?(if_exists=true) table =
  let b = Buffer.create 30 in
  Printf.bprintf b "DROP TABLE%s%s;"
    (if if_exists then " IF EXISTS " else "")
    (table_name table);
  let request = Buffer.contents b in
  Connection.exec conn ~expect:[Postgresql.Command_ok] request
    (fun _ -> Deferred.unit)

let get column row =
  column.ty.t_of_sql (row.result#getvalue row.index (row.offset + column.index))

let add_row = (|>)

module SQL = struct

  type ('table,'kind) from =
    | From: 'table table * int -> ('table,'kind) from

  type rel = string

  type 'p term =
    | Get: ('table,_) from * (_,'p,'table) column -> 'p term
    | Param: (_,'p) Ty.t * int -> 'p term
    | Op2: rel * 'p term * 'p term -> 'p term
    | Op1: rel * 'p term -> 'p term

  type formula =
    | And: formula * formula -> formula
    | Or: formula * formula -> formula
    | Not: formula -> formula
    | Rel: rel * 'p term * 'p term -> formula

  type modify =
    | Modify: ('table,[`UPDATED]) from * ('a,'p,'table) column * 'p term
      -> modify

  module Array = struct
    let get: ('table,_) from -> ('a,'p,'table) column -> 'p term =
      fun from col -> Get(from,col)
    let set from col v = Modify(from,col,v)

  end

  type _ result =
    | Row1: ('table,_) from -> ('table row) result
    | Row2: ('table1,_) from * ('table2,_) from ->
      ('table1 row * 'table2 row) result

  let string_of_rel x = x

  let sql_result: type r. Buffer.t -> r result -> unit = fun b result ->
    let rec aux i b = function
      | Nil -> ()
      | Cons(col,Nil) -> Printf.bprintf b "t%i.%s" i col.name
      | Cons(col,l) -> Printf.bprintf b "t%i.%s, " i col.name; aux i b l in
    match result with
    | Row1 (From(t1,i)) -> aux i b t1.columns
    | Row2 (From(t1,i1),From(t2,i2)) ->
      aux i1 b t1.columns; Buffer.add_string b ", "; aux i2 b t2.columns

  let rec sql_term b = function
    | Get(From(_,i),col) -> Printf.bprintf b "t%i.%s" i col.name
    | Param(_,i) -> Printf.bprintf b "$%i" (i+1)
    | Op2(s,t1,t2) -> Printf.bprintf b "(%a %s %a)" sql_term t1 s sql_term t2
    | Op1(s,t1) -> Printf.bprintf b "(%s %a)" s sql_term t1

  let rec sql_formula b = function
    | And(f1,f2) ->
      Printf.bprintf b "(%a and %a)" sql_formula f1 sql_formula f2
    | Or(f1,f2) ->
      Printf.bprintf b "(%a or %a)" sql_formula f1 sql_formula f2
    | Not f1->
      Printf.bprintf b "(not %a)" sql_formula f1
    | Rel(rel,t1,t2) -> Printf.bprintf b "%a %s %a"
                          sql_term t1 (string_of_rel rel) sql_term t2

  let sql_where b formula = sql_formula b formula

  let sql_modify b l =
    let f = function
      | Modify(_,col,term) ->
        Printf.bprintf b "%s = %a" col.name sql_term term
    in
    List.iter ~f l

  let result1 t = Row1(t)
  let result2 t1 t2 = Row2(t1,t2)


  let (=)  t1 t2 = Rel("=",t1,t2)
  let (<)  t1 t2 = Rel("<",t1,t2)
  let (<=) t1 t2 = Rel("<=",t1,t2)
  let (>)  t1 t2 = Rel(">",t1,t2)
  let (>=) t1 t2 = Rel(">=",t1,t2)
  let (<>) t1 t2 = Rel("<>",t1,t2)

  type 'a num = [< `INT | `FLOAT] as 'a
  let (+) t1 t2 = Op2("+",t1,t2)
  let (-) t1 t2 = Op2("-",t1,t2)
  let ( * ) t1 t2 = Op2("*",t1,t2)
  let ( ** ) t1 t2 = Op2("^",t1,t2)
  let (mod) t1 t2 = Op2("%",t1,t2)
  let (/) t1 t2 = Op2("/",t1,t2)

  let (land) t1 t2 = Op2("&",t1,t2)
  let (lor) t1 t2 = Op2("|",t1,t2)
  let (lxor) t1 t2 = Op2("#",t1,t2)
  let lnot t1 = Op1("|",t1)
  let (lsl) t1 t2 = Op2("<<",t1,t2)
  let (lsr) t1 t2 = Op2(">>",t1,t2)

  let (&&) f1 f2 = And(f1,f2)
  let (||) f1 f2 = Or(f1,f2)
  let not f1 = Not f1

  module Build = struct
    let rel s t1 t2 = Rel(s,t1,t2)
    let op2 s t1 t2 = Op2(s,t1,t2)
    let op1 s t1 = Op1(s,t1)
  end

end

module Params = struct
  type ('inner_arg,'outer_arg,'inner_res,'outer_res) t =
    | Nil: ('inner_res,'outer_res,'inner_res,'outer_res) t
    | Cons: ('a,'p) Ty.t * ('inner_arg,'outer_arg,'inner_res,'outer_res) t ->
      ('p SQL.term -> 'inner_arg,'a -> 'outer_arg,'inner_res,'outer_res) t

  let rec nbarg : type a b c d. int -> (a,b,c,d) t -> int = fun acc l ->
    match l with
    | Nil -> acc
    | Cons (_,l) -> nbarg (acc+1) l

  let e = Nil
  let (@) ty l = Cons(ty,l)

end

module From = struct
  type (_,_) t =
    | Nil: ('params,'params) t
    | Cons: 'table table * ('params,'formula) t ->
      ('params, ('table,[`USED]) SQL.from -> 'formula) t

  let e = Nil
  let (@) t l = Cons(t,l)

  let to_sql i b from =
    let rec aux: type a b. bool -> int -> Buffer.t -> (a,b) t -> unit =
      fun first i b -> function
        | Nil -> ()
        | Cons(table,from) ->
          Printf.bprintf b "%s %s as t%i"
            (if first then "From" else ",")
            (table_name table) i;
          aux false (i+1) b from in
    aux true i b from

end

let rec foldi f acc max min =
  if min > max then acc else foldi f (f acc max) (max - 1) min

let extract_result : type r. r SQL.result -> Postgresql.result -> r list =
  fun spec result  ->
    match spec with
    | SQL.Row1 _ ->
      foldi (fun acc index -> {result; index; offset = 0}::acc)
        [] (result#ntuples - 1) 0
    | SQL.Row2 (SQL.From (t1,_),_) ->
      let nbt1 = nbcolumns 0 t1.columns in
      foldi (fun acc index -> ({result; index; offset = 0},
                               {result; index; offset = nbt1})::acc)
        [] (result#ntuples - 1) 0

let rec doparam:
  type inner_arg outer_arg inner_res outer_res.
  compute_request:(inner_res -> Connection.t -> string Array.t -> outer_res) ->
  (inner_arg,outer_arg,inner_res,outer_res) Params.t ->
  int -> (** number of previous argument *)
  inner_arg ->
  Connection.t -> string Array.t -> outer_arg
  = fun ~compute_request params iparam formula ->
    match params with
    | Params.Nil -> compute_request formula
    | Params.Cons(ty,params) ->
      let call =
        doparam ~compute_request params
          (iparam+1) (formula (SQL.Param(ty,iparam))) in
      (fun conn array ->
         (fun x ->
            array.(iparam) <- (ty.sql_of_t x);
            call conn array))

(** print the sql from *)
let rec dofrom: type inner_arg inner_from outer_arg inner_res outer_res.
  compute_request:(inner_res -> Connection.t -> string Array.t -> outer_res) ->
  (inner_arg,inner_from) From.t ->
  int -> (** index of this from *)
  (inner_arg,outer_arg, inner_res, outer_res) Params.t ->
  inner_from ->
  Connection.t -> outer_arg
  = fun ~compute_request from ifrom params formula ->
    match from with
    | From.Nil ->
      let call =
        doparam ~compute_request params 0 formula in
      let nbarg = Params.nbarg 0 params in
      (fun conn ->
         let array = Array.create ~len:nbarg "" in
         call conn array)
    | From.Cons(table,from) ->
      dofrom ~compute_request
        from (ifrom+1) params (formula (SQL.From (table,ifrom)))

let select: type inner_arg inner_from outer_arg r.
  from:(inner_arg,inner_from) From.t ->
  param:(inner_arg,outer_arg,
         SQL.formula * r SQL.result,
         (r list) exn_ret_defer) Params.t ->
  inner_from ->
  Connection.t -> outer_arg
  = fun ~from ~param formula ->
    let compute_request (formula,result) =
      let request =
        let b = Buffer.create 20 in
        Printf.bprintf b "SELECT %a %a WHERE %a;"
          SQL.sql_result result
          (From.to_sql 0) from
          SQL.sql_where formula;
        Buffer.contents b in
      fun conn array ->
        Connection.exec conn
          ~expect:[Postgresql.Tuples_ok]
          ~params:array
          request
          (fun res -> Deferred.return (extract_result result res))
    in
    dofrom ~compute_request from 0 param formula

type 'result select = Connection.t -> 'result

let exec_select = (|>)

type 'result update = Connection.t -> 'result

let update:
  type inner_arg inner_from outer_arg table'.
  from:(inner_arg,inner_from) From.t ->
  param:(inner_arg,outer_arg,
         SQL.formula * SQL.modify list,
         Int63.t exn_ret_defer) Params.t ->
  update:table' table ->
  ((table',[`UPDATED]) SQL.from -> inner_from) ->
  Connection.t ->
  outer_arg
  = fun ~from ~param ~update formula ->
    let compute_request (formula,modify) =
      let request =
        let b = Buffer.create 20 in
        Printf.bprintf b "UPDATE %s as t%i SET %a %a WHERE %a;"
          (table_name update) 0
          SQL.sql_modify modify
          (From.to_sql 1) from
          SQL.sql_where formula;
        Buffer.contents b in
      fun conn array ->
        Connection.exec conn
          ~expect:[Postgresql.Command_ok]
          ~params:array
          request
          (fun res ->
             assert (res#ntuples = 0 && res#nfields = 0);
             Deferred.return (Int63.of_string res#cmd_tuples))
    in
    let formula = formula (SQL.From (update,0)) in
    dofrom ~compute_request from 1 param formula


let exec_update = (|>)
