open Csp
open Guedra

let init () =
  let (wch, pch) = create_toplevel_window "Guedra" 0 0 768 512 in
  let rch = make_chan () in
  let cch = make_chan () in
  let nch = make_chan () in
  let wch0 = make_chan () in
  let wch1 = make_chan () in
  par [
      (fun () -> Hsplitter.init Splitter.Movable wch pch cch nch rch wch0 wch1 300 1 4);
      (fun () -> Window.init wch0 rch);
      (fun () -> Window.init wch1 rch)]

let () = reg_client init
