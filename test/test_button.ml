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
  let rch = make_chan () in
  let cwch0 = make_chan () in
  let cwch1 = make_chan () in
  let cch0 = make_chan () in
  let cch1 = make_chan () in
  let nch = make_chan () in
  let cs = [(cwch0, rect 50 50 100 40);
            (cwch1, rect 50 120 100 40)]
  in
  par [
      (fun () -> Form.init wch pch rch cs);
      (fun () -> Button.init cwch0 rch cch0 nch "OK");
      (fun () -> Button.init cwch1 rch cch1 nch "Cancel");
      (fun () -> main nch 0)]

let () = reg_client init
