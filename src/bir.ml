open !Core_kernel
open Bap.Std

let create_var (dst : Ast.dst) : Var.t =
  let name = Ast.dst_reg_name dst in
  Var.create name reg8_t

let create_int_exp (n : Ast.num) : Exp.t =
  let i = Ast.int_of_num n in
  let word = Word.of_int i ~width:8 in
  Bil.Types.Int word

let create_var_exp (reg : Ast.reg) : Exp.t =
  let var = Var.create (Ast.reg_str reg) reg8_t in
  Bil.Types.Var var

let create_value_exp (src : Ast.src) : Exp.t =
  match src with
  | Ast.Src_num n -> create_int_exp n 
  | Ast.Src_reg r -> create_var_exp r

let create_addition_exp (src1 : Ast.src) (src2 : Ast.src) : Exp.t =
  let src1' = create_value_exp src1 in
  let src2' = create_value_exp src2 in
  Bil.Types.BinOp (Bil.Types.PLUS, src1', src2')

let create_assignment
    (dst : Ast.dst) (exp : Exp.t) (tid : Tid.t) (addr : Addr.t) : Def.t =
  let variable = create_var dst in
  let term = Def.create variable exp ~tid in
  Term.set_attr term address addr

let create_jump
    (cond : Exp.t) (target : Exp.t) (tid : Tid.t) (addr : Addr.t) : Jmp.t =
  let term = Jmp.create (Goto (Indirect target)) ~cond ~tid in
  Term.set_attr term address addr

let append_stmnt
    (blk : Blk.t) (stmnt : Ast.stmnt) (labels : Labels.t) : Blk.t =
  let label = Ast.stmnt_label stmnt in
  let tid = Labels.tid_of label labels in
  let addr = Labels.addr_of label in
  match Ast.stmnt_cmd stmnt with
  | Ast.Cmd_mov (Ast.Mov (src, dst)) ->
    let exp = create_value_exp src in
    let term = create_assignment dst exp tid addr in
    Term.append def_t blk term
  | Ast.Cmd_add (Ast.Add (src1, src2, dst)) ->
    let exp = create_addition_exp src1 src2 in
    let term = create_assignment dst exp tid addr in
    Term.append def_t blk term 
  | Ast.Cmd_jmp (Ast.Jmp (src1, src2)) ->
    let cond = create_value_exp src1 in
    let target = create_value_exp src2 in
    let term = create_jump cond target tid addr in
    Term.append jmp_t blk term

let create_blk (block : Ast.block) (labels : Labels.t) : Blk.t = 
  let label = Ast.block_label block in
  let tid = Labels.tid_of label labels in
  let blk = Blk.create () ~tid:tid in
  let stmnts = Ast.block_stmnts block in
  List.fold stmnts ~init:blk 
    ~f:(fun blk' stmnt -> append_stmnt blk' stmnt labels)

let create_prog (tree : Ast.t) (labels : Labels.t) : Program.t =
  let prog = Program.create () in
  let main = Sub.create () ~name:"main" in
  let blks = List.map tree ~f:(fun block -> create_blk block labels) in
  let main'' = List.fold blks ~init:main
    ~f:(fun main' blk -> Term.append blk_t main' blk) in
  Term.append sub_t prog main''

let lift (tree : Ast.t) (labels : Labels.t) : Program.t = 
  create_prog tree labels
