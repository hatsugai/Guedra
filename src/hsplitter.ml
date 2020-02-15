open Csp
open Guedra

type capture = NoCap | Client of win_msg chan | Border of int

type context = {
    mutable width : int;
    mutable height : int;
    mutable boundary : int;
    mutable inv : event option;
    mutable cap : capture;
    mutable act : bool;
    mutable cact : win_msg chan option;
  }

let init splitter_type wch pch cch nch rch cwch0 cwch1 boundary border_width play =

  let pque = Queue.create () in
  let nque = Queue.create () in

  let cc = {
      width = 0;
      height = 0;
      boundary = boundary;
      inv = None;
      cap = NoCap;
      act = false;
      cact = None;
    }
  in

  let rec process () =
    let event_list =
      [ recvEvt wch always win_msg;
        recvEvt rch always win_req;
        recvEvt cch always win_cmd ]
    in select_que3 event_list cc.inv pque nque

  and win_msg msg =
    match msg with
      Paint (x, y, w, h) ->
       paint x y w h
    | WinSize (w, h) ->
       cc.width <- w;
       cc.height <- h;
       (if w <= border_width then
          (cc.boundary <- 0; inv ())
        else if w <= cc.boundary + border_width then
          (cc.boundary <- w - border_width; inv ()));
       resize ()
    | MouseDown (x, y, state, button) ->
       if splitter_type = Splitter.Movable &&
            x >= cc.boundary - play && x < cc.boundary + border_width + play then
         (cc.cap <- Border (cc.boundary - x); process ())
       else if x < cc.boundary then
         (cc.cap <- Client cwch0;
          send cwch0 (MouseDown (x, y, state, button)) process)
       else
         (cc.cap <- Client cwch1;
          send cwch1
            (MouseDown (x - cc.boundary - border_width, y, state, button))
            process)
    | MouseUp (x, y, state, button) ->
       (match cc.cap with
          NoCap -> process ()
        | Client ch ->
           let x =
             if ch == cwch0 then
               x
             else
               x - cc.boundary - border_width
           in
           cc.cap <- NoCap;
           send ch (MouseUp (x, y, state, button)) process
        | Border delta ->
           cc.cap <- NoCap;
           let boundary = delta + x in
           if boundary >= 0 &&
                boundary < cc.height - border_width then
             (cc.boundary <- delta + x;
              inv ();
              resize ())
           else
             process ())
    | MouseMove (x, y, state) ->
       (match cc.cap with
          NoCap -> process ()
        | Client ch ->
           let x =
             if ch == cwch0 then
               x
             else
               x - cc.boundary - border_width
           in
           send ch (MouseMove (x, y, state)) process
        | Border delta ->
           let boundary = delta + x in
           if boundary >= 0 &&
                boundary < cc.width - border_width then
             (cc.boundary <- boundary;
              inv ();
              resize ())
           else
             process ())
    | MouseWheel (x, y, state, direction) ->
       if x < cc.boundary then
         send cwch0 msg process
       else
         let d = cc.boundary + border_width in
         send cwch1 (MouseWheel (x - d, y, state, direction)) process
    | Scroll (x, y, state, dx, dy) ->
       if x < cc.boundary then
         send cwch0 msg process
       else
         let d = cc.boundary + border_width in
         send cwch1 (Scroll (x - d, y, state, dx, dy)) process
    | Scale scale ->
       send cwch0 msg
         (fun () ->
           send cwch1 msg process)
    | KeyDown (key, state) ->
       (match cc.cact with
          None -> process ()
        | Some ch ->
           send ch msg process)
    | KeyUp (key, state) ->
       (match cc.cact with
          None -> process ()
        | Some ch ->
           send ch msg process)
    | Active b ->
       cc.act <- b;
       (match cc.cact with
          None -> process ()
        | Some cwch ->
           send cwch msg process)
    | _ -> process ()

  and win_req (ch, msg) =
    match msg with
      Invalidate (x, y, w, h) ->
       (match cc.inv with
          None ->
           let msg =
             if ch == cwch0 then
               Invalidate (x, y, w, h)
             else
               Invalidate (x + cc.boundary + border_width, y, w, h)
           in
           let e = sendEvt pch (wch, msg) clear_inv in
           cc.inv <- Some e;
           process ()
        | Some _ -> invalidate ())
    | Activate ->
       if cc.act then
         match cc.cact with
           Some cwch ->
            if cwch == ch then
              process ()
            else
              send cwch (Active false)
                (fun () ->
                  send ch (Active true)
                    (fun () ->
                      cc.cact <- Some ch; process ()))
         | None ->
            send ch (Active true)
              (fun () ->
                cc.cact <- Some ch; process ())
       else
         (cc.cact <- Some ch;
          Queue.add (sendEvt pch (wch, Activate) pque_drop) pque;
          process ())
    | _ -> process ()

  and win_cmd msg =
    match msg with
      Splitter.SetBorder pos ->
      if pos >= 0 && pos < cc.width - border_width then
        (cc.boundary <- pos; resize ())
      else
        process ()

  and resize () =
    send cwch0 (WinSize (cc.boundary, cc.height))
      (fun () ->
        let w = max 0 (cc.width - cc.boundary - border_width) in
        send cwch1 (WinSize (w, cc.height)) invalidate)

  and paint x y w h =
    seq [
        (fun () ->
          (* cwch0 *)
          push_translate 0.0 0.0;
          push_clip 0 0 cc.boundary cc.height;
          send cwch0 (Paint (x, y, w, h))
            (fun () ->
              recv rch (guard_paintack cwch0)
                (fun _ ->
                  pop_clip ();
                  pop_translate ();
                  skip ())));
        (fun () ->
          (* cwch1 *)
          let d = cc.boundary + border_width in
          push_translate (flo d) 0.0;
          push_clip 0 0
            (cc.width - d)
            cc.height;
          send cwch1
            (Paint (x - d, y, w, h))
            (fun () ->
              recv rch (guard_paintack cwch1)
                (fun _ ->
                  pop_clip ();
                  pop_translate ();
                  skip ())));
        (fun () ->
          (* border *)
          set_color o_o.color_fore;
          fill_rect (float_of_int cc.boundary) 0.0
            (float_of_int border_width)
            (float_of_int cc.height);
          send pch (wch, PaintAck) process)]

  and clear_inv () =
    cc.inv <- None;
    process ()

  and inv () =
    let msg = (wch, Invalidate (0, 0, cc.width, cc.height)) in
    let e = sendEvt pch msg clear_inv in
    cc.inv <- Some e

  and invalidate () =
    inv ();
    process ()

  and pque_drop () = let _ = Queue.take pque in process ()

  in process ()
