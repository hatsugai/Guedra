open Csp
open Guedra

type 'a context = {
    mutable width : int;
    mutable height : int;
    mutable fWidth : float;
    mutable fHeight : float;
    mutable inv : event option;
}

let init wch pch cch nch modifier_list =

  let vscroll_width = o_o.scroll_thumb_size in

  let rch = make_chan () in
  let rch_hsp = make_chan () in
  let wch_hsp = make_chan () in
  let wch_stxt = make_chan () in
  let wch_vs = make_chan () in
  let cch_hsp = make_chan () in
  let cch_stxt = make_chan () in
  let cch_vs = make_chan () in
  let nch_hsp = make_chan () in
  let nch_stxt = make_chan () in
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
        recvEvt nch_stxt always handle_stxt;
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
           send cch_hsp (Splitter.SetBorder border) process)
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
      StaticText.SetText text ->
       send cch_stxt msg
         (fun () ->
           send cch_stxt StaticText.QuerySize
             (fun () ->
               recv nch_stxt (fun (ch, _) -> ch == cch_stxt)
                 (fun (_, msg) ->
                   match msg with
                     StaticText.AckSize (w, h) ->
                      send cch_vs (Scroll.SetRange (h, 0)) process
                    |_ -> process ())))

    | _ -> process ()

  and handle_stxt (ch, msg) =     (* from Checklist *)
    match msg with
      Pos (x, y) ->
       notify msg;
       send cch_vs (Scroll.SetIndex y) process
    | _ -> process ()

  and handle_vs (ch, msg) =     (* from Vscroll *)
    match msg with
      Scroll.Pos pos ->
      send cch_stxt (StaticText.SetPos (0, pos)) process

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
                   wch_vs wch_stxt
                   200 0 0);
      (fun () -> StaticText.init wch_stxt rch_hsp cch_stxt nch_stxt modifier_list);
      (fun () -> Vscroll.init wch_vs rch_hsp cch_vs nch_vs 100 10 0);
      process ]
