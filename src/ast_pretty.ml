open !Core_kernel

let string_of_label label : string =
  match label with Ast.Label i -> Format.sprintf "@0x%-6x" i

let string_of_num num : string =
  match num with Ast.Hex i -> Format.sprintf "0x%-6x" i

let string_of_reg reg : string =
  let r = match reg with
    | Ast.R1 -> "reg.1"
    | Ast.R2 -> "reg.2"
    | Ast.R3 -> "reg.3"
    | Ast.R4 -> "reg.4"
    in
  Format.sprintf "%-8s" r

let string_of_src src : string =
  match src with
  | Ast.Src_num num -> string_of_num num
  | Ast.Src_reg reg -> string_of_reg reg

let string_of_dst dst : string =
  match dst with
  | Ast.Dst_reg reg -> string_of_reg reg

let string_of_cmd cmd : string =
  match cmd with
  | Ast.Cmd_mov (Ast.Mov (src, dst)) -> Format.sprintf "%-8s %s %s"
      "mov" (string_of_src src) (string_of_dst dst)
  | Ast.Cmd_add (Ast.Add (src1, src2, dst)) -> Format.sprintf "%-8s %s %s %s"
      "add" (string_of_src src1) (string_of_src src2) (string_of_dst dst)
  | Ast.Cmd_jmp (Ast.Jmp (src1, src2)) -> Format.sprintf "%-5s if:%s %s"
      "jmp" (string_of_src src1) (string_of_src src2)

let string_of_stmnt stmnt : string =
  match stmnt with
  | Ast.Stmnt (label, cmd) ->
    Format.sprintf "%s%s" (string_of_label label) (string_of_cmd cmd)

let string_of_block block : string =
  match block with
  | Ast.Block (label, stmnts) ->
    let block_pretty = Format.sprintf
        "%sBLOCK..........................." (string_of_label label) in
    let stmnts_pretty = List.map stmnts ~f:string_of_stmnt in
    String.concat ~sep:"\n" (block_pretty :: stmnts_pretty)

let to_string t : string =
  let blocks_pretty = List.map t ~f:string_of_block in
  String.concat ~sep:"\n\n" blocks_pretty
