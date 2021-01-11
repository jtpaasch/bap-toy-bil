open !Core_kernel
open Bap.Std

type t = Tid.t Int.Map.t

let empty : t = Int.Map.empty

let add (label : Ast.label) t : t =
  Int.Map.set t ~key:(Ast.int_of_label label) ~data:(Tid.create ())

let tid_of (label : Ast.label) t : Tid.t =
  Int.Map.find_exn t (Ast.int_of_label label)

let addr_of (label : Ast.label) : Addr.t =
  Addr.of_int (Ast.int_of_label label) ~width:8

let append_stmnt_label (labels : t) (stmnt : Ast.stmnt) : t =
  let label = Ast.stmnt_label stmnt in
  add label labels

let append_block_labels (labels : t) (block : Ast.block) : t =
  let label = Ast.block_label block in
  let labels' = add label labels in
  let stmnts = Ast.block_stmnts block in
  List.fold stmnts ~init:labels' ~f:append_stmnt_label

let create (tree : Ast.t) : t =
  List.fold tree ~init:empty ~f:append_block_labels

let to_string t : string =
  let data = Int.Map.to_alist t in
  let strings = List.map data ~f:(fun (key, data) ->
    Format.sprintf "@0x%x -> %s" key (Tid.to_string data)) in
  String.concat ~sep:"\n" strings
