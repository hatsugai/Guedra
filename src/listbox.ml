open Csp
open Csp
open Guedra

type ('a, 'd) custom = {
    item_height : int;
    (* x y width height cs i sel ud act *)
    draw : int -> int -> int -> int -> 'a array -> int -> int -> ('a, 'd) custom -> 'd -> bool -> unit;
    item_string : 'a -> string;
  }

type 'a command =
  Select of int
| SetList of ('a array * int) (* sel *)
| SetPos of int
| QuerySize

type 'a notification =
  OnSelect of 'a array * int
| OnAction of 'a array * int
| OnPosChanged of int
| AckSize of int * int * int (* num_items, num_lines, pos *)

type ('a, 'd) context = {
    mutable cs : 'a array;
    mutable ud : ('a, 'd) custom;
    mutable width : int;
    mutable height : int;
    mutable fWidth : float;
    mutable fHeight : float;
    mutable act : bool;
    mutable pos : int;
    mutable sel : int;
    mutable inv : event option;
  }

let interval_fold a b acc f =
  let rec fold acc k =
    if k >= b then
      acc
    else
      fold (f acc k) (k+1)
  in fold acc a

let normalize_pos pos height item_height num_items =
  let num_lines = (height + item_height - 1) / item_height in
  let max_pos = num_items - num_lines in
  if pos < 0 then
    0
  else if pos > max_pos then
    max 0 max_pos
  else
    pos

let normalize_sel sel num_items =
  if sel >= 0 && sel < num_items then
    sel
  else
    -1                          (* indicates "no select" *)

let adjust_pos pos sel height item_height num_items =
  if sel >= 0 then
    let num_lines = (height + item_height - 1) / item_height in
    if num_items = 0 then
      0
    else if sel >= pos && sel < pos + num_lines then
      pos
    else
      normalize_pos sel height item_height num_items
  else
    pos

let init wch pch cch nch ud dc =
  let pque = Queue.create () in
  let nque = Queue.create () in
  let cc = {
      ud = ud;
      cs = [||];
      width = 0;
      height = 0;
      fWidth = 0.0;
      fHeight = 0.0;
      act = false;
      pos = 0;
      sel = -1;
      inv = None;
    }
  in

  let rec process () =
    let event_list =
      [ recvEvt wch always win_msg;
        recvEvt cch always win_cmd ]
    in
    select_que3 event_list cc.inv pque nque

  and win_msg msg =
    match msg with
      MouseDown (x, y, state, button) ->
       mouse_down x y state button
    | MouseWheel (x, y, state, direction) ->
       mouse_wheel direction
    | KeyDown (key, state) ->
       key_down key state
    | Active b ->
       cc.act <- b; invalidate ()
    | Paint (x, y, w, h) ->
       paint x y w h
    | WinSize (w, h) ->
       cc.width <- w;
       cc.height <- h;
       cc.fWidth <- float_of_int w;
       cc.fHeight <- float_of_int h;
       let num_items = Array.length cc.cs in
       cc.pos <- normalize_pos cc.pos cc.height ud.item_height num_items;
       process ()
    | _ -> process ()

  and win_cmd msg =
    match msg with
      Select sel ->
       cc.sel <- sel;
       invalidate ()
    | SetList (cs, sel) ->
       cc.cs <- cs;
       cc.pos <- if sel >= 0 then sel else 0;
       cc.sel <- sel;
       invalidate ()
    | SetPos pos ->
       cc.pos <- pos;
       invalidate ()
    | QuerySize ->
       let num_items = Array.length cc.cs in
       let num_lines = cc.height / ud.item_height in
       send nch (cch, AckSize (num_items, num_lines, cc.pos)) process

  and mouse_down x y state button =
    inv ();
    Queue.add (sendEvt pch (wch, Activate) pque_drop) pque;
    let i = y / cc.ud.item_height in
    let index = cc.pos + i in
    if i >= 0 && index < Array.length cc.cs then
      (if index = cc.sel then (
         if is_double_clicked state then (
           notify (OnAction (cc.cs, index));
           process ())
         else
           process ())
       else (
         cc.sel <- index;
         notify (OnSelect (cc.cs, index));
         invalidate ()))
    else
      process ()

  and mouse_wheel direction =
    let num_items = Array.length cc.cs in
    let num_lines = cc.height / ud.item_height in
    let pos =
      if direction > 0 then
        max 0 (cc.pos - 1)
      else
        max 0 (min (cc.pos + 1) (num_items - num_lines))
    in
    cc.pos <- pos;
    notify (OnPosChanged pos);
    invalidate ()

  and key_down key state =
    let num_items = Array.length cc.cs in
    let num_lines = cc.height / ud.item_height in
    if key = keyUp || key = keyDown then
      let sel =
        if cc.sel >= 0 then
          if key = keyUp then
            if cc.sel > 0 then cc.sel - 1 else cc.sel
          else
            if cc.sel < num_items - 1 then cc.sel + 1 else cc.sel
        else
          if num_items > 0 then 0 else -1
      in
      let pos =
        if sel >= 0 then
          if sel < cc.pos then
            sel
          else if sel >= cc.pos && sel < cc.pos + num_lines then
            cc.pos
          else
            sel - num_lines + 1
        else
          cc.pos
      in
      if sel = cc.sel then
        process ()
      else
        (notify (OnSelect (cc.cs, sel));
         cc.sel <- sel;
         cc.pos <- pos;
         notify (OnPosChanged pos);
         invalidate ())
    else if key = keyPageDown then
      let pos = max 0 (min (cc.pos + num_lines) (num_items - num_lines)) in
      cc.pos <- pos;
      notify (OnPosChanged pos);
      invalidate ()
    else if key = keyPageUp then
      let pos = max 0 (cc.pos - num_lines) in
      cc.pos <- pos;
      notify (OnPosChanged pos);
      invalidate ()
    else if key = keyHome then
      (cc.pos <- 0;
       notify (OnPosChanged cc.pos);
       invalidate ())
    else if key = keyEnd then
      let pos = max 0 (num_items - num_lines) in
      cc.pos <- pos;
      notify (OnPosChanged pos);
      invalidate ()
    else if key = keyEscape then
      (cc.sel <- -1;
       notify (OnSelect (cc.cs, cc.sel));
       invalidate ())
    else if key = 99 && (state land keyStateMask) = keyStateControl then
         let s =
           interval_fold 0 (Array.length cc.cs) ""
             (fun s i ->
               let item = Array.get cc.cs i in
               s ^ "\n" ^ (ud.item_string item))
         in
         clipboard_set_text s;
         process ()
    else
      process ()

  and paint x y w h =
    let x_ofs = int_of_float o_o.left_margin in
    let rec loop i y =
      if i < Array.length cc.cs && y < cc.height then
        (ud.draw x_ofs y cc.width ud.item_height cc.cs i cc.sel ud dc cc.act;
         loop (i + 1) (y + ud.item_height))
    in
    set_color o_o.color_back;
    fill_rect 0.0 0.0 cc.fWidth cc.fHeight;
    set_font o_o.font;
    loop cc.pos 0;
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

  and pque_drop () = let _ = Queue.take pque in process ()
  and nque_drop () = let _ = Queue.take nque in process ()

  in process ()
