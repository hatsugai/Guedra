open Csp
open Guedra

type 'a command =
  SetText of string list
| SetPos of int * int
| QuerySize

type 'a notification =
  Pos of int * int
| AckSize of int * int

type modifier = Reverse | Number

type capture =
  CapNone
| CapScroll of (int * int) * (int * int)

type ('a, 'd) context = {
    mutable width : int;
    mutable height : int;
    mutable fWidth : float;
    mutable fHeight : float;
    mutable inv : event option;
    mutable text : string list;
    mutable pos_x : int;
    mutable pos_y : int;
    mutable text_width : int;
    mutable text_height : int;
    mutable cap : capture;
    mutable mod_reverse : bool;
    mutable mod_number : bool;
  }

let calc_text_size text =
  let rec loop w h text =
    match text with
      [] -> (w, h)
    | s::text ->
       let extents = text_extents s in
       loop (max w extents.width) (h +. extents.height) text
  in
  set_font o_o.font;
  loop 0.0 0.0 text

let init wch pch cch nch modifier_list =
  let pque = Queue.create () in
  let nque = Queue.create () in
  let cc = {
      width = 0;
      height = 0;
      fWidth = 0.0;
      fHeight = 0.0;
      inv = None;
      text = [];
      pos_x = 0;
      pos_y = 0;
      text_width = 0;
      text_height = 0;
      cap = CapNone;
      mod_reverse = false;
      mod_number = false;
    }
  in

  let rec parse_modifiers ms =
    match ms with
      [] -> ()
    | m::ms ->
       (match m with
          Reverse -> cc.mod_reverse <- true
        | Number -> cc.mod_number <-true);
       parse_modifiers ms
  in

  let rec process () =
    let event_list =
      [ recvEvt wch always win_msg;
        recvEvt cch always win_cmd ]
    in
    select_que3 event_list cc.inv pque nque

  and win_msg msg =
    match msg with
      Paint (x, y, w, h) ->
       paint x y w h
    | WinSize (w, h) ->
       cc.width <- w;
       cc.height <- h;
       cc.fWidth <- float_of_int w;
       cc.fHeight <- float_of_int h;
       process ()
    | MouseDown (x, y, state, button) ->
       Queue.add (sendEvt pch (wch, Activate) pque_drop) pque;
       if button = mouseButtonRight then
         (cc.cap <- CapScroll ((x, y), (x, y));
          process ())
       else
         process ()
    | MouseMove (x, y, state) ->
       (match cc.cap with
          CapNone -> process ()
        | CapScroll ((x0, y0), _) ->
           cc.cap <- CapScroll ((x0, y0), (x, y));
           invalidate ())
    | MouseUp (x, y, state, button) ->
       (match cc.cap with
          CapNone -> process ()
        | CapScroll ((x0, y0), _) ->
           cc.cap <- CapNone;
           cc.pos_x <- max 0 (min (cc.pos_x - x + x0) cc.text_width);
           cc.pos_y <- max 0 (min (cc.pos_y - y + y0) cc.text_height);
           notify (Pos (cc.pos_x, cc.pos_y));
           invalidate ())
    | MouseWheel (x, y, state, direction) ->
       if direction > 0 then
         (cc.pos_y <- max 0 (min (cc.pos_y + (int_of_float o_o.funit)) cc.text_height);
          invalidate ())
       else
         (cc.pos_y <- max 0 (min (cc.pos_y - (int_of_float o_o.funit)) cc.text_height);
          invalidate ())
    | Scroll (x, y, state, dx, dy) ->
       cc.pos_y <- max 0 (min (cc.pos_y + dy) cc.text_height);
       notify (Pos (cc.pos_x, cc.pos_y));
       invalidate ()
    | KeyDown (code, state) ->
       if code = 99 && (state land keyStateMask) = keyStateControl then
         let s =
           List.fold_left
             (fun s t -> s ^ "\n" ^ t)
             "" cc.text
         in
         clipboard_set_text s;
         process ()
       else
         process ()
    | _ -> process ()

  and win_cmd msg =
    match msg with
     SetText text ->
       cc.text <- if cc.mod_reverse then List.rev text else text;
       let (w, h) = calc_text_size cc.text in
       cc.text_width <- int_of_float w;
       cc.text_height <- int_of_float h;
       cc.pos_x <- 0; cc.pos_y <- 0;
       notify (Pos (cc.pos_x, cc.pos_y));
       invalidate ()
    | SetPos (x, y) ->
       cc.pos_x <- x; cc.pos_y <- y;
       invalidate ()
    | QuerySize ->
       send nch (cch, AckSize (cc.text_width, cc.text_height)) process

  and paint x y w h =
    let (x0, y0) =
      match cc.cap with
        CapNone -> (cc.pos_x, cc.pos_y)
      | CapScroll ((x0, y0), (x1, y1)) ->
         (cc.pos_x + x0 - x1, cc.pos_y + y0 - y1)
    in
    let xf = float_of_int x0 in
    let yf = float_of_int y0 in
    set_color o_o.color_back;
    fill_rect 0.0 0.0 cc.fWidth cc.fHeight;
    set_color o_o.color_fore;
    set_font o_o.font;
    if cc.mod_number then
      List.iteri
        (fun i s ->
          draw_text
            (o_o.left_margin -. xf)
            (o_o.top_margin -. yf +. (float_of_int i) *. o_o.funit)
            (Printf.sprintf "%2d %s" i s))
        cc.text
    else
      List.iteri
        (fun i s ->
          draw_text
            (o_o.left_margin -. xf)
            (o_o.top_margin -. yf +. (float_of_int i) *. o_o.funit)
            s)
        cc.text;
    send pch (wch, PaintAck) process

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

  and nque_drop () = let _ = Queue.take nque in process ()
  and pque_drop () = let _ = Queue.take pque in process ()

  in
  parse_modifiers modifier_list;
  process ()
