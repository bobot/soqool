
open Core.Std
open Async.Std
open Table

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


let todo =
  Connection.create ~dbname:"oci_tests_table" ()
  >>= fun res ->
  let conn = Result.ok_exn res in
  table_exists conn Author.t
  >>= fun res ->
  let _tab = Result.ok_exn res in
  Deferred.return ()


let same_name =
  select
    ~from:From.(Author.t @ Author.t @ e)
    ~param:Params.(Ty.string @ e)
    SQL.(fun author1 author2 name ->
        author1.(Author.name) = name && author2.(Author.name) = name,
        result2 author1 author2)

let same_name conn name : (string * string) list Deferred.t =
  exec_select conn same_name name
  >>= fun res ->
  let l = Result.ok_exn res in
  Deferred.return
    (List.map l ~f:(fun (res1,res2) -> (get Author.firstname res1,
                                        get Author.firstname res2)))


let () =
  don't_wait_for todo;
  never_returns (Scheduler.go ())
