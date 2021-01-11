open !Core_kernel

let reg_of (s : string) : Ast.reg =
  match s with
  | "r1" -> Ast.R1
  | "r2" -> Ast.R2
  | "r3" -> Ast.R3
  | "r4" -> Ast.R4
  | _ -> failwith (Format.sprintf "Invalid register: %s" s)

let is_reg (s : string) : bool =
  try let _ = reg_of s in true
  with Failure _ -> false

let int_of_hex (s : string) : int =
  try Scanf.sscanf s "0x%x%!" (fun x -> x)
  with Scanf.Scan_failure _ -> failwith (Format.sprintf "Invalid hex: %s" s)

let is_hex (s : string) : bool =
  try let _ = int_of_hex s in true
  with Failure _ -> false

let parse_label (s : string) : Ast.label =
  match is_hex s with
  | true -> Ast.Label (int_of_hex s)
  | false -> failwith (Format.sprintf "Invalid label (must be hex): %s" s)

let parse_dst (s : string) : Ast.dst =
  match is_reg s with
  | true -> Ast.Dst_reg (reg_of s)
  | false -> failwith
      (Format.sprintf "Invalid dst (must be register): %s" s)

let parse_src (s : string) : Ast.src =
  match is_reg s with
  | true -> Ast.Src_reg (reg_of s)
  | false ->
    begin
      match is_hex s with
      | true -> Ast.Src_num (Ast.Hex (int_of_hex s))
      | false -> failwith 
          (Format.sprintf "Invalid src (must be hex or register): %s" s)
    end

let parse_stmnt (sexp : Sexp.t) : Ast.stmnt =
  match sexp with
  | Sexp.List [ Atom label; Atom "mov"; Atom src; Atom dst ] ->
    let label' = parse_label label in
    let src' = parse_src src in
    let dst' = parse_dst dst in
    Ast.Stmnt (label', Ast.Cmd_mov (Ast.Mov (src', dst')))
  | Sexp.List [ Atom label; Atom "add"; Atom src1; Atom src2; Atom dst ] ->
    let label' = parse_label label in
    let src1' = parse_src src1 in
    let src2' = parse_src src2 in
    let dst' = parse_dst dst in
    Ast.Stmnt (label', Ast.Cmd_add (Ast.Add (src1', src2', dst')))
  | Sexp.List [ Atom label; Atom "jmp"; Atom src1; Atom src2 ] ->
    let label' = parse_label label in
    let src1' = parse_src src1 in
    let src2' = parse_src src2 in
    Ast.Stmnt (label', Ast.Cmd_jmp (Ast.Jmp (src1', src2')))
  | _ -> failwith (Format.asprintf "Invalid statement: %a" Sexp.pp sexp)

let parse_block (label : string) (sexps : Sexp.t list) : Ast.block =
  let label' = parse_label label in
  let stmnts = List.map sexps ~f:parse_stmnt in
  Ast.Block (label', stmnts)

let rec parse ?blocks:(blocks=[]) (sexps : Sexp.t list) : Ast.t =
  match sexps with
  | [] -> blocks
  | head :: tail ->
    begin
      match head with
      | Sexp.List (Atom label :: Atom "block" :: body) ->
        let block = parse_block label body in
        let blocks' = List.append blocks [block] in
        parse tail ~blocks:blocks'
      | _ -> failwith
        (Format.asprintf "Error. Expected block: %a" Sexp.pp head)
    end
