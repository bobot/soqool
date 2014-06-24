

module Connection = Soqool_core.Connection

module MkTable = Soqool_core.MkTable

module RowId = Soqool_core.Id

module From = struct
  let nil = Soqool_core.From.nil
  let (@) = Soqool_core.From.cons
end

module Params = struct
  module Ty = Soqool_ty.Ty
  let nil = Soqool_core.Params.nil
  let (@) = Soqool_core.Params.cons
end


let create_table = Soqool_core.create_table
let drop_table = Soqool_core.drop_table
let add_row = Soqool_core.add_row
let row_get = Soqool_core.get

let select = Soqool_core.select
let exec_select = Soqool_core.exec_select

let update = Soqool_core.update
let exec_update = Soqool_core.exec_update

module SQL = struct
  include Soqool_ty
  let (+) = Int63.(+)
  let (-) = Int63.(+)
  let ( * ) = Int63.( * )
  let (/) = Int63.(/)
  let (+.) = Float.(+)
  let (-.) = Float.(-)
  let ( *. ) = Float.( * )
  let (/.) = Float.(/)
  let ( ** ) = Float.( ** )

  include Soqool_poly
  include Soqool_sql
end
