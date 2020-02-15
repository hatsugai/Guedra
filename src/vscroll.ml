open Csp
open Guedra
open Scroll

let init wch pch cch nch range0 page0 index0 =
  let pque = Queue.create () in
  let nque = Queue.create () in
  let cc = make_cc range0 page0 index0 in

  let rec process () =
    let event_list =
      [ recvEvt wch always win_msg;
        recvEvt cch always win_cmd ]
    in
    select_que3 event_list cc.inv pque nque

  and win_msg msg =
    match msg with
      Paint (x, y, w, h) ->
       (* background *)
       set_color o_o.color_back;
       fill_rect 0.0 0.0 cc.fWidth cc.fHeight;
       (* thumb *)
       (if cc.range > 0 && cc.height > o_o.scroll_thumb_size then (
          set_color o_o.color_handle;
          fill_rect 0.0 (flo cc.pos) cc.fWidth (flo o_o.scroll_thumb_size)));
       send pch (wch, PaintAck) process
    | WinSize (w, h) ->
       cc.width <- w;
       cc.height <- h;
       cc.fWidth <- float_of_int w;
       cc.fHeight <- float_of_int h;
       let p = calc_pos h cc.range cc.index in
       cc.pos <- p;
       invalidate ()
    | MouseDown (x, y, state, button) ->
       request Activate;
       if y < cc.pos then (
         (* page up *)
         let i = max 0 (cc.index - cc.page) in
         let p = calc_pos cc.height cc.range i in
         cc.index <- i;
         cc.pos <- p;
         notify (Pos i);
         invalidate ())
       else if y >= cc.pos + o_o.scroll_thumb_size then (
         (* page down *)
         let i = min (cc.index + cc.page) cc.range in
         let p = calc_pos cc.height cc.range i in
         cc.index <- i;
         cc.pos <- p;
         notify (Pos i);
         invalidate ())
       else if cc.height > o_o.scroll_thumb_size && cc.range > 0 then (
         (* thumb *)
         cc.capture <- true;
         cc.displacement <- cc.pos - y;
         process ())
       else
         process ()
  
    | MouseUp (x, y, state, button) ->
       if cc.capture then
         let p = max 0
                   (min (y + cc.displacement)
                      (cc.height - o_o.scroll_thumb_size))
         in
         let i = calc_index cc.height cc.range p in
         let p = calc_pos cc.height cc.range i in
         cc.pos <- p;
         cc.index <- i;
         cc.capture <- false;
         notify (Pos i);
         invalidate ()
       else
         process ()

    | MouseMove (x, y, state) ->
       if cc.capture then
         let p = max 0
                   (min (y + cc.displacement)
                      (cc.height - o_o.scroll_thumb_size))
         in
         let i = calc_index cc.height cc.range p in
         let p = calc_pos cc.height cc.range i in
         cc.pos <- p;
         cc.index <- i;
         notify (Pos i);
         invalidate ()
       else
         process ()
    | _ -> process ()

  and win_cmd msg =
    match msg with
      SetIndex i ->
       assert (i >= 0 && i <= cc.range);
       cc.index <- i;
       cc.pos <- calc_pos cc.height cc.range i;
       invalidate ()
    | SetRange (r, i) ->
       assert (r >= 0 && i >= 0 && i <= r);
       cc.range <- r;
       cc.index <- i;
       cc.pos <- calc_pos cc.height r i;
       invalidate ()

  and request msg =
    Queue.add (sendEvt pch (wch, msg) pque_drop) pque;

  and notify msg =
    Queue.add (sendEvt nch (cch, msg) nque_drop) nque

  and inv () =
    let msg = (wch, Invalidate (0, 0, cc.width, cc.height)) in
    let e = sendEvt pch msg clear_inv in
    cc.inv <- Some e

  and invalidate () =
    inv ();
    process ()

  and clear_inv () =
    cc.inv <- None;
    process ()

  and pque_drop () = let _ = Queue.take pque in process ()
  and nque_drop () = let _ = Queue.take nque in process ()

  in process ()
