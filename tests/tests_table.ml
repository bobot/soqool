
open Core.Std
open Async.Std
open Table

let verbose = true

module Author = struct
  module T = MkTable(struct
      let name = "author"
      let version = 1
    end)

  let uc   = T.uc
  let name, uc = T.add_column ~name:"name" Ty.string uc
  let firstname, uc = T.add_column ~name:"firstname" Ty.string uc

  let t, add, id = T.close uc
end


let create_conn () =
  Connection.create ~verbose ~dbname:"oci_tests_table" ()
  >>= fun res ->
  let conn = Result.ok_exn res in
  create_table conn Author.t
  >>= fun res ->
  let () = Result.ok_exn res in
  Deferred.return conn

let authors = ["foo","bar"; "foo","fu"]

let populate conn =
  Deferred.List.iter ~f:(fun (name,firstname) ->
      add_row conn Author.add name firstname
        >>= (fun id ->
          let id = Result.ok_exn id in
          printf "%s\n" (Id.to_string id);
          Deferred.unit
    )) authors

let same_name =
  select
    ~from:From.(Author.t @ Author.t @ e)
    ~param:Params.(Ty.string @ e)
    SQL.(fun author1 author2 name ->
        author1.(Author.name) = name && author2.(Author.name) = name,
        result2 author1 author2)

let same_name name conn () =
  exec_select conn same_name name
  >>= fun res ->
  let l = Result.ok_exn res in
  Deferred.return l


let print_names l =
  printf "print_names:\n";
  List.iter l ~f:(fun (res1,res2) ->
      assert (String.equal (get Author.name res1) (get Author.name res1));
      printf "%s: %s --- %s\n"
        (get Author.name res1)
        (get Author.firstname res1) (get Author.firstname res2));
  Deferred.unit

let () =
  don't_wait_for (
  create_conn () >>= fun conn ->
  populate conn >>=
  same_name "foo" conn >>=
  print_names >>= fun () ->
  Table.drop_table conn Author.t >>= fun res ->
  let () = Result.ok_exn res in
  Shutdown.exit 0);
  never_returns (Scheduler.go ())
