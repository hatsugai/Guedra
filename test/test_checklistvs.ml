open Csp
open Guedra

let interval_map a b f =
  let rec g rs k =
    if k >= b then
      List.rev rs
    else
      g ((f k)::rs) (k + 1)
  in g [] a

let item_string _ _ = ""

let draw_item x y width height cs i sel ud dc act b_check =
  (if i = sel then
     (set_color o_o.color_fore;
      fill_rect (flo x) (flo y) (flo width) (flo height);
      set_color o_o.color_back)
   else
     set_color o_o.color_fore);
  let label = Array.get cs i in
  draw_text (flo x) (flo y) label

let init () =
  let (wch, pch) = create_toplevel_window "Guedra" 0 0 768 512 in
  let rch = make_chan () in
  let wch_clvs = make_chan () in
  let cch_clvs = make_chan () in
  let nch_clvs = make_chan () in
  let cs = [(wch_clvs, rect 50 50 300 400)] in
  let ud = {
      Checklist.item_height = 18;
      draw = draw_item;
      item_string = item_string;
    }
  in

  let rec process () =
    recv nch_clvs always
      (fun (cch, msg) ->        (* from Checklist(vs) *)
        match msg with
          Checklist.OnSelect (cs, i) ->
          Printf.printf "sel: %d\n" i;
          flush stdout;
          process ()
        | OnCheck (cs, i, b) ->
           Printf.printf "check: %d %s\n" i (if b then "on" else "off");
           flush stdout;
           process ()
        | OnAction (cs, i, b) ->
           Printf.printf "action: %d\n" i;
           flush stdout;
           process ()
        | _ -> process ())

  and main () =
    let xs = interval_map 0 100 (fun i -> Printf.sprintf "item %d (@#$%%&,.?MWgqy|)" i) in
    let v = Array.of_list xs in
    send cch_clvs (Checklist.SetList (v, Checklist.IntMap.empty, -1)) process

  in
  par [
      (fun () -> Form.init wch pch rch cs);
      (fun () -> Checklistvs.init wch_clvs rch cch_clvs nch_clvs ud ());
      main]

let () = reg_client init
