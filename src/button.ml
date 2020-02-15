open Csp
open Guedra

type request = SetText of string
type notification = Command

type context = {
    mutable text : string;
    mutable width : int;
    mutable height : int;
    mutable fWidth : float;
    mutable fHeight : float;
    mutable capture : bool;
    mutable press : bool;
    mutable inv : event option;
  }

let color_btn = color 0.7 0.7 0.75
let color_btn_text = color 0.0 0.0 0.0
let color_btn_shadow = color 0.3 0.3 0.3

let draw_3d_frame_up x y w h =
  set_line_width 1.0;
  set_color color_white;
  draw_line (x +. 0.5) (y +. 0.5) (x +. w -. 0.5) (y +. 0.5);
  draw_line (x +. 0.5) (y +. 0.5) (x +. 0.5) (y +. h -. 0.5);
  set_color color_btn_shadow;
  draw_line (x +. w -. 0.5) (y +. 0.5) (x +. w -. 0.5) (y +. h -. 0.5);
  draw_line (x +. 0.5) (y +. h -. 0.5) (x +. w -. 0.5) (y +. h -. 0.5)

let draw_3d_frame_down x y w h =
  set_line_width 1.0;
  set_color color_btn_shadow;
  draw_line (x +. 0.5) (y +. 0.5) (x +. w -. 0.5) (y +. 0.5);
  draw_line (x +. 0.5) (y +. 0.5) (x +. 0.5) (y +. h -. 0.5);
  set_color color_white;
  draw_line (x +. w -. 0.5) (y +. 0.5) (x +. w -. 0.5) (y +. h -. 0.5);
  draw_line (x +. 0.5) (y +. h -. 0.5) (x +. w -. 0.5) (y +. h -. 0.5)

let draw_3d_frame x y w h down =
  if down then
     draw_3d_frame_down x y w h
   else
     draw_3d_frame_up x y w h

let init wch pch cch nch text =
  let pque = Queue.create () in
  let nque = Queue.create () in
  let cc = {
      text = text;
      width = 0;
      height = 0;
      fWidth = 0.0;
      fHeight = 0.0;
      capture = false;
      press = false;
      inv = None;
    }
  in

  let rec process () =
    let event_list = [
        recvEvt wch always win_msg;
        recvEvt cch always win_cmd;
      ]
    in
    select_que3 event_list cc.inv pque nque

  and win_msg msg =
    match msg with
      Paint (x, y, w, h) ->
       (* background *)
       set_color color_btn;
       fill_rect 0.0 0.0 cc.fWidth cc.fHeight;
       (* text *)
       set_font o_o.font;
       let m = text_extents cc.text in
       let x = 0.5 *. (cc.fWidth -. m.width)
       and y = 0.5 *. (cc.fHeight -. m.height)
       in
       set_color color_btn_text;
       (if cc.press then
          let offset = o_o.font_size *. 0.08 in
          draw_text (x +. offset) (y +. offset) cc.text
        else
          draw_text x y cc.text);
       (* 3D frame *)
       draw_3d_frame 1.0 1.0 (cc.fWidth -. 2.0) (cc.fHeight -. 2.0) cc.press;
       (* outer frame *)
       set_color color_black;
       set_line_width 1.0;
       draw_rect 0.5 0.5 (cc.fWidth -. 1.0) (cc.fHeight -. 1.0);
       send pch (wch, PaintAck) process
    | WinSize (w, h) ->
       cc.width <- w;
       cc.height <- h;
       cc.fWidth <- float_of_int w;
       cc.fHeight <- float_of_int h;
       process ()
    | MouseDown (x, y, state, button) ->
       request Activate;
       cc.capture <- true;
       cc.press <- true;
       invalidate ()
    | MouseUp (x, y, state, button) ->
       (if cc.capture && cc.press then
          notify Command);
       cc.capture <- false;
       cc.press <- false;
       invalidate ()
    | MouseMove (x, y, state) ->
       if cc.capture
          && ((x >= 0 && x < cc.width && y >= 0 && y < cc.height
               && not cc.press)
              || (not (x >= 0 && x < cc.width && y >= 0 && y < cc.height)
                  && cc.press))
       then
         (cc.press <- not cc.press; invalidate ())
       else
         process ()
    | _ -> process ()

  and win_cmd msg =
    match msg with
      SetText text ->
      cc.text <- text;
      invalidate ()

  and notify msg =
    Queue.add (sendEvt nch (cch, msg) nque_drop) nque

  and request msg =
    Queue.add (sendEvt pch (wch, msg) pque_drop) pque;

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
