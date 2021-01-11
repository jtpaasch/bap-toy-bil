type label = Label of int
type num = Hex of int
type reg = R1 | R2 | R3 | R4

type src = Src_num of num | Src_reg of reg
type dst = Dst_reg of reg

type mov = Mov of src * dst
type add = Add of src * src * dst
type jmp = Jmp of src * src

type cmd = 
  | Cmd_mov of mov 
  | Cmd_add of add
  | Cmd_jmp of jmp

type stmnt = Stmnt of label * cmd
type block = Block of label * stmnt list

type t = block list

let int_of_label (label : label) : int =
  match label with Label i -> i

let int_of_num (num : num) : int =
  match num with Hex i -> i

let reg_str (reg : reg) : string =
  match reg with
  | R1 -> "r1"
  | R2 -> "r2"
  | R3 -> "r3"
  | R4 -> "r4"

let dst_reg_name (dst : dst) : string =
  match dst with Dst_reg r -> reg_str r

let stmnt_label (stmnt : stmnt) : label =
  match stmnt with Stmnt (label, _) -> label

let stmnt_cmd (stmnt : stmnt) : cmd =
  match stmnt with Stmnt (_, cmd) -> cmd

let block_label (block : block) : label =
  match block with Block (label, _) -> label

let block_stmnts (block : block) : stmnt list =
  match block with Block (_, stmnts) -> stmnts
