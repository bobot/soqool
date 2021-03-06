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
    (Postgresql.result -> 'a Deferred.t) ->
    ('a, exn) Core.Std.Result.t Deferred.t

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
  | Cons: ('a,'rt) column * 'rt columns -> 'rt columns

and ('a,'rt) column = {
  ty  : 'a ty;
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

and 'a ty = {
  kind: 'a kind;
  sql_of_t: 'a -> string;
  t_of_sql: string -> 'a;
}

let rec nbcolumns acc = function
  | Nil -> acc
  | Cons(_,l) -> nbcolumns (acc+1) l

module Ty = struct

  type 'a t = 'a ty

  (** TODO binary format *)

  let from_sexp sexp_of_t t_of_sexp = {
    kind = Data Postgresql.TEXT;
    sql_of_t = (fun x -> Sexp.to_string (sexp_of_t x));
    t_of_sql = (fun x -> t_of_sexp (Sexp.of_string x));
  }

  module MkSexp (S:Sexpable.S)= struct
    let t = from_sexp S.sexp_of_t S.t_of_sexp
  end

  module Obj = struct
    let mk_with_string kind sql_of_t t_of_sql = {
      kind = Data kind;
      sql_of_t;
      t_of_sql;
    }

  end


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
    | Cons: 'a ty * ('add,'a -> 'res)  uc -> ('add,'res) uc

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
    t table * add adder * (t id,t) column =
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

module Formula = struct

  type ('table,'kind) from =
    | From: 'table table * int -> ('table,'kind) from

  type rel = string

  type 'p term =
    | Get: ('table,_) from * (_,'table) column -> 'p term
    | Param: _ Ty.t * int -> 'p term
    | Op2: rel * 'p term * 'p term -> 'p term
    | Op1: rel * 'p term -> 'p term

  type formula =
    | FOp2: rel * formula * formula -> formula
    | FOp1: rel * formula -> formula
    | FRel: rel * 'p term * 'p term -> formula

  type modify =
    | Modify: ('table,[`UPDATED]) from * ('a,'table) column * 'p term
      -> modify

  let get_term: ('table,_) from -> ('a,'table) column -> 'p term =
    fun from col -> Get(from,col)
  let set_term from col v = Modify(from,col,v)

  let string_of_rel x = x

  let rec sql_term b = function
    | Get(From(_,i),col) -> Printf.bprintf b "t%i.%s" i col.name
    | Param(_,i) -> Printf.bprintf b "$%i" (i+1)
    | Op2(s,t1,t2) -> Printf.bprintf b "(%a %s %a)" sql_term t1 s sql_term t2
    | Op1(s,t1) -> Printf.bprintf b "(%s %a)" s sql_term t1

  let rec sql_formula b = function
    | FOp2(rel,f1,f2) ->
      Printf.bprintf b "(%a %s %a)" sql_formula f1 rel sql_formula f2
    | FOp1(rel,f1)->
      Printf.bprintf b "(%s %a)" rel sql_formula f1
    | FRel(rel,t1,t2) -> Printf.bprintf b "%a %s %a"
                          sql_term t1 (string_of_rel rel) sql_term t2

  let sql_where b formula = sql_formula b formula

  let sql_modify b l =
    let f = function
      | Modify(_,col,term) ->
        Printf.bprintf b "%s = %a" col.name sql_term term
    in
    List.iter ~f l

  module Obj = struct
    let op2 s t1 t2 = Op2(s,t1,t2)
    let op1 s t1 = Op1(s,t1)

    let rel s t1 t2 = FRel(s,t1,t2)
    let fop2 s t1 t2 = FOp2(s,t1,t2)
    let fop1 s t1 = FOp1(s,t1)

  end

end


module Return = struct
  open Formula

  type 'a value =
    | Get: ('table,_) from * ('a,'table) column -> 'a value

  let get: ('table,_) from -> ('a,'table) column -> 'a value =
    fun from col -> Get(from,col)

  type ('arg,'res) ft =
    | Nil: ('res,'res) ft
    | Row: ('table,_) from  * ('arg, 'res) ft
      -> ('table row -> 'arg, 'res) ft
    | Value: 'a value * ('arg, 'res) ft
      -> ('a -> 'arg, 'res) ft

  type 'res t =
    | Result: 'a * ('a,'b) ft -> 'b t

  let rec sql_ft: type arg res. Buffer.t -> (arg,res) ft -> unit =
    fun b result ->
      let rec sql_row i b : _ columns -> unit = function
        | Nil -> ()
        | Cons(col,Nil) -> Printf.bprintf b "t%i.%s"   i col.name
        | Cons(col,l)   -> Printf.bprintf b "t%i.%s, " i col.name;
          sql_row i b l
      in
      let print_comma : type arg res. Buffer.t -> (arg,res) ft -> unit =
        fun b -> function
          | Nil     -> ()
          | Row _   -> Buffer.add_string b ", "
          | Value _ -> Buffer.add_string b ", "
      in
      match result with
      | Nil -> ()
      | Row(From(t,i),l) ->
        sql_row i b t.columns; print_comma b l; sql_ft b l
      | Value(Get(From(_,i),col),l) ->
        Printf.bprintf b "t%i.%s, " i col.name;
        print_comma b l;
        sql_ft b l

  let sql_result: type r. Buffer.t -> r t -> unit = fun b -> function
    | Result (_,ft) -> sql_ft b ft

  let return ft f = Result(f,ft)
  let value  v l  = Value(v,l)
  let row f l  = Row(f,l)
  let nil = Nil
end

module Params = struct
  type ('inner_arg,'outer_arg,'inner_res,'outer_res) t =
    | Nil: ('inner_res,'outer_res,'inner_res,'outer_res) t
    | Cons: 'a Ty.t * ('inner_arg,'outer_arg,'inner_res,'outer_res) t ->
      ('p Formula.term -> 'inner_arg,'a -> 'outer_arg,'inner_res,'outer_res) t

  let rec nbarg : type a b c d. int -> (a,b,c,d) t -> int = fun acc l ->
    match l with
    | Nil -> acc
    | Cons (_,l) -> nbarg (acc+1) l

  let nil = Nil
  let cons ty l = Cons(ty,l)

end

module From = struct
  type (_,_) t =
    | Nil: ('params,'params) t
    | Cons: 'table table * ('params,'formula) t ->
      ('params, ('table,[`USED]) Formula.from -> 'formula) t

  let nil = Nil
  let cons t l = Cons(t,l)

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

let extract_result : type r. r Return.t -> Postgresql.result -> r list =
  fun spec result  ->
    let rec sql_ft: type arg res. arg -> (arg,res) Return.ft -> offset:int
      -> (Postgresql.result -> int -> res) =
        fun f ft ~offset ->
          match ft with
          | Return.Nil -> (fun _ _ -> f)
          | Return.Row(Formula.From(t,_),l) ->
            let nbt = nbcolumns 0 t.columns in
            fun result index ->
              sql_ft
                (f {result; index; offset})
                l ~offset:(offset + nbt)
                result index
          | Return.Value(Return.Get(_,col),l) ->
            fun result index ->
              sql_ft
                (f (col.ty.t_of_sql (result#getvalue index offset)))
                l ~offset:(offset + 1)
                result index
      in
    match spec with
    | Return.Result (f,ft) ->
      foldi (fun acc index -> (sql_ft f ft ~offset:0 result index)::acc)
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
          (iparam+1) (formula (Formula.Param(ty,iparam))) in
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
        from (ifrom+1) params (formula (Formula.From (table,ifrom)))

let select: type inner_arg inner_from outer_arg r.
  from:(inner_arg,inner_from) From.t ->
  param:(inner_arg,outer_arg,
         Formula.formula * r Return.t,
         (r list) exn_ret_defer) Params.t ->
  inner_from ->
  Connection.t -> outer_arg
  = fun ~from ~param formula ->
    let compute_request (formula,result) =
      let request =
        let b = Buffer.create 20 in
        Printf.bprintf b "SELECT %a %a WHERE %a;"
          Return.sql_result result
          (From.to_sql 0) from
          Formula.sql_where formula;
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
         Formula.formula * Formula.modify list,
         Int63.t exn_ret_defer) Params.t ->
  update:table' table ->
  ((table',[`UPDATED]) Formula.from -> inner_from) ->
  Connection.t ->
  outer_arg
  = fun ~from ~param ~update formula ->
    let compute_request (formula,modify) =
      let request =
        let b = Buffer.create 20 in
        Printf.bprintf b "UPDATE %s as t%i SET %a %a WHERE %a;"
          (table_name update) 0
          Formula.sql_modify modify
          (From.to_sql 1) from
          Formula.sql_where formula;
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
    let formula = formula (Formula.From (update,0)) in
    dofrom ~compute_request from 1 param formula


let exec_update = (|>)
