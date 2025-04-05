let () =
  let shape = Shaper.parse_channel stdin in
  Format.printf "%a@." Shaper.Shape.pp_sexp shape
