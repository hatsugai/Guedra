open Csp
open Guedra

let ppi = ref 96.0
let font_name = ref "monospace"
let font_size = ref 10.0

let option_spec =
  [
    ("-ppi", Arg.Float  (fun x -> ppi := x), "");
    ("-fn",  Arg.String (fun s -> font_name := s), "");
    ("-fs",  Arg.Float  (fun x -> font_size := x), "");
  ]

let init () =
  let (wch, rch) = create_toplevel_window "Guedra" 0 0 512 384 in
  Window.init wch rch

let () =
  Arg.parse option_spec (fun s -> ()) "";
  reg_client init ~ppi:!ppi ~font_name:!font_name ~font_size:!font_size
