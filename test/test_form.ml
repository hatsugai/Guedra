open Csp
open Guedra

let rec main nch k =
  recv nch always
    (fun (cch, msg) ->
      match msg with
        Button.Command ->
         prn (Printf.sprintf "btn cmd %d %d\n" (chid cch) k);
         send cch (Button.SetText (Printf.sprintf "Btn %d" k))
           (fun () -> main nch (k + 1)))

let init () =
  let (wch, pch) = create_toplevel_window "Guedra" 0 0 768 512 in
  let rch0 = make_chan () in
  let rch1 = make_chan () in
  let cwch0 = make_chan () in
  let cwch1 = make_chan () in
  let cch = make_chan () in
  let nch = make_chan () in
  let cs0 = [(cwch0, rect 50 50 400 300)] in
  let cs1 = [(cwch1, rect 50 50 100 40)] in
  par [
      (fun () -> Form.init wch pch rch0 cs0);
      (fun () -> Form.init cwch0 rch0 rch1 cs1);
      (fun () -> Button.init cwch1 rch1 cch nch "OK");
      (fun () -> main nch 0)]

let () = reg_client init
