let () =
  match Sys.argv with
  | [| _ |] ->
      let shape = Shaper.parse_channel stdin in
      Format.printf "%a@." Shaper.Shape.pp_sexp shape
  | [| _; file_name |] ->
      let shape = Shaper.parse_channel ~file_name (open_in file_name) in
      Format.printf "%a@." Shaper.Shape.pp_sexp shape
  | _ -> failwith "invalid usage"
