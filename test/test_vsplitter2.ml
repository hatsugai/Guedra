open Csp
open Guedra

let init () =
  let (wch, pch) = create_toplevel_window "Guedra" 0 0 768 512 in
  let rch0 = make_chan () in
  let rch1 = make_chan () in
  let cch0 = make_chan () in
  let cch1 = make_chan () in
  let nch = make_chan () in
  let cwch0 = make_chan () in
  let cwch1 = make_chan () in
  let cwch2 = make_chan () in
  let cwch3 = make_chan () in
  par [
      (fun () -> Vsplitter.init Splitter.Movable
                   wch pch cch0 nch rch0 cwch0 cwch1 200 4 4);
      (fun () -> Vsplitter.init Splitter.Movable
                   cwch0 rch0 cch1 nch rch1 cwch2 cwch3 200 4 4);
      (fun () -> Window.init cwch1 rch0);
      (fun () -> Window.init cwch2 rch1);
      (fun () -> Window.init cwch3 rch1)]

let () = reg_client init
