open Csp
open Guedra

type 'a context = {
    mutable width : int;
    mutable height : int;
    mutable fWidth : float;
    mutable fHeight : float;
    mutable inv : event option;
}

let init wch pch cch nch ud dc =

  let vscroll_width = o_o.scroll_thumb_size in

  let rch = make_chan () in
  let rch_hsp = make_chan () in
  let wch_hsp = make_chan () in
  let wch_lb = make_chan () in
  let wch_vs = make_chan () in
  let cch_hsp = make_chan () in
  let cch_lb = make_chan () in
  let cch_vs = make_chan () in
  let nch_hsp = make_chan () in
  let nch_lb = make_chan () in
  let nch_vs = make_chan () in

  let pque = Queue.create () in
  let nque = Queue.create () in

  let cc = {
      width = 0;
      height = 0;
      fWidth = 0.0;
      fHeight = 0.0;
      inv = None;
    }
  in

  let rec process () =
    let event_list =
      [ recvEvt wch always win_msg;
        recvEvt rch always win_req;
        recvEvt cch always win_cmd;
        recvEvt nch_lb always handle_lb;
        recvEvt nch_vs always handle_vs ]
    in select_que3 event_list cc.inv pque nque

  and win_msg msg =
    match msg with
    | WinSize (w, h) ->
       cc.width <- w;
       cc.height <- h;
       cc.fWidth <- float_of_int w;
       cc.fHeight <- float_of_int h;
       let border =
         vscroll_width
           (*if w >= vscroll_width then w - vscroll_width else 0*)
       in
       send wch_hsp msg
         (fun () ->
           send cch_hsp (Splitter.SetBorder border)
             (fun () ->
               send cch_lb Listbox.QuerySize
                 (fun () ->
                   recv nch_lb always
                     (fun (_, ack) ->
                       match ack with
                         Listbox.AckSize (items, lines, pos) ->
                          let range = max 0 (items - lines) in
                          send cch_vs (Scroll.SetRange (range, pos)) process
                        |_ -> process ()))))

    | Paint _ ->
       send wch_hsp msg
         (fun () ->
           recv rch (guard_paintack wch_hsp)
             (fun _ -> send pch (wch, PaintAck) process))
    | _ ->
       send wch_hsp msg process

  and win_req (ch, msg) =
    match msg with
      Invalidate _ ->
       inv (); process ()
    | Activate ->
       Queue.add (sendEvt pch (wch, Activate) pque_drop) pque;
       process ()
    | _ -> process ()

  and win_cmd msg =
    match msg with
      Listbox.SetList (v, sel) ->
       send cch_lb msg
         (fun () ->
           send cch_lb Listbox.QuerySize
             (fun () ->
               recv nch_lb (fun (ch, _) -> ch == cch_lb)
                 (fun (_, msg) ->
                   match msg with
                     Listbox.AckSize (items, lines, pos) ->
                      let range = max 0 (items - lines) in
                      send cch_vs (Scroll.SetRange (range, pos)) process
                    |_ -> process ())))

    | Listbox.Select sel ->
       send cch_lb msg process
    | Listbox.QuerySize ->
       send cch_lb msg
         (fun () ->
           recv nch_lb always
             (fun (_, msg) ->
               send nch (cch, msg) process))
    | _ -> process ()

  and handle_lb (ch, msg) =     (* from Listbox *)
    match msg with
      Listbox.OnSelect (cs, sel) ->
       notify msg;
       process ()
    | Listbox.OnPosChanged pos ->
       notify msg;
       send cch_vs (Scroll.SetIndex pos) process
    | Listbox.OnAction (cs, index) ->
       notify msg;
       process ()
    | _ -> process ()

  and handle_vs (ch, msg) =     (* from Vscroll *)
    match msg with
      Scroll.Pos pos ->
      send cch_lb (Listbox.SetPos pos) process

  and notify msg =
    Queue.add (sendEvt nch (cch, msg) nque_drop) nque

  and inv () =
    let msg = (wch, Invalidate (0, 0, cc.width, cc.height)) in
    let e = sendEvt pch msg clear_inv in
    cc.inv <- Some e

  and clear_inv () =
    cc.inv <- None;
    process ()

  and pque_drop () = let _ = Queue.take pque in process ()
  and nque_drop () = let _ = Queue.take nque in process ()

  in
  par
    [ (fun () -> Hsplitter.init Splitter.Fixed wch_hsp rch cch_hsp nch_hsp rch_hsp
                   wch_vs wch_lb
                   200 0 0);
      (fun () -> Listbox.init wch_lb rch_hsp cch_lb nch_lb ud dc);
      (fun () -> Vscroll.init wch_vs rch_hsp cch_vs nch_vs 100 10 0);
      process ]
