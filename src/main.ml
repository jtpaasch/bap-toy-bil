open !Core_kernel
open Bap.Std

let get_arg_or_exit ~arg ~error =
  if (Array.length Sys.argv) < (arg + 1) then
    begin
      print_endline error;
      exit 1
    end
  else Sys.argv.(arg)

let () =
  let error = "Error: specify a filepath, e.g., resources/test.toy" in
  let filename = get_arg_or_exit ~arg:1 ~error in

  Format.printf "-- Loading file: %s\n\n%!" filename;
  let source = Sexp.load_sexps filename in

  let tree = Parser.parse source in
  Format.printf "-- Parsed:\n%s\n\n%!" (Ast_pretty.to_string tree);

  let labels = Labels.create tree in
  Format.printf "-- LABELS:\n%s\n\n" (Labels.to_string labels);

  let bir = Bir.lift tree labels in
  Format.printf "-- BIR:\n%s%!" (Program.to_string bir);

  Format.printf "\n-- Done\n%!"
