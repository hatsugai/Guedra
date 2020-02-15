open Csp
open Guedra

let iota n =
  let rec loop acc i =
    if i=0 then acc else loop (i::acc) (i-1)
  in loop [] n

let owner cch nch =
  let text =
    List.fold_left
      (fun text i -> (Printf.sprintf "%d hello, world" i)::text)
      [] (iota 20)
  in
  send cch (StaticText.SetText text) skip

let init () =
  let (wch, pch) = create_toplevel_window "Guedra" 0 0 768 512 in
  let rch = make_chan () in
  let cwch0 = make_chan () in
  let cch0 = make_chan () in
  let nch = make_chan () in
  let cs = [(cwch0, rect 50 50 320 240)]
  in
  par [
      (fun () -> Form.init wch pch rch cs);
      (fun () -> StaticTextVs.init cwch0 rch cch0 nch []);
      (fun () -> owner cch0 nch);
    ]

let () = reg_client init
