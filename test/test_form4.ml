open Csp
open Guedra

let init () =
  let (wch, pch) = create_toplevel_window "Guedra" 0 0 768 512 in
  let rch = make_chan () in
  let rch_form1 = make_chan () in
  let rch_form2 = make_chan () in
  let wch_form1 = make_chan () in
  let wch_form2 = make_chan () in
  let wch1 = make_chan () in
  let wch2 = make_chan () in
  let wch3 = make_chan () in
  let wch4 = make_chan () in
  let cs0 = [(wch_form1, rect 50  50 600 300);
             (wch_form2, rect 50 400 600 300)] in
  let cs1 = [(wch1, rect  50 50 200 100);
             (wch2, rect 300 50 200 100)] in
  let cs2 = [(wch3, rect  50 50 200 100);
             (wch4, rect 300 50 200 100)] in
  par [
      (fun () -> Form.init wch pch rch cs0);
      (fun () -> Form.init wch_form1 rch rch_form1 cs1);
      (fun () -> Form.init wch_form2 rch rch_form2 cs2);
      (fun () -> Window.init wch1 rch_form1);
      (fun () -> Window.init wch2 rch_form1);
      (fun () -> Window.init wch3 rch_form2);
      (fun () -> Window.init wch4 rch_form2) ]

let () = reg_client init
