open Csp
open Guedra

let init () =
  let (wch, pch) = create_toplevel_window "Guedra" 0 0 768 512 in
  let rch = make_chan () in
  let cch = make_chan () in
  let nch = make_chan () in
  let cwch0 = make_chan () in
  let cwch1 = make_chan () in
  par [
      (fun () -> Vsplitter.init Splitter.Movable wch pch cch nch rch cwch0 cwch1 300 4 4);
      (fun () -> Window.init cwch0 rch);
      (fun () -> Window.init cwch1 rch)]

let () = reg_client init
