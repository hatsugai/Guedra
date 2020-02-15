open Csp
open Guedra

let rec main nch =
  let rec process () =
    recv nch always
      (fun (cch, msg) ->
        match msg with
          Scroll.Pos i ->
          Printf.printf "index: %d\n" i;
          flush stdout;
          process ())
  in process ()

let init () =
  let (wch, pch) = create_toplevel_window "Guedra" 0 0 768 512 in
  let rch = make_chan () in
  let cwch0 = make_chan () in
  let cch = make_chan () in
  let nch = make_chan () in
  let cs = [(cwch0, rect 50 50 300 30)]
  in
  par [
      (fun () -> Form.init wch pch rch cs);
      (fun () -> Hscroll.init cwch0 rch cch nch 100 10 0);
      (fun () -> main nch)]

let () = reg_client init
