open Csp
open Guedra

module IntMap =
  Map.Make(
      struct
        type t = int
        let compare x y = x - y
      end)

type ('a, 'd) custom = {
    item_height : int;
    (* x y width height cs i sel ud act b_check *)
    draw : int -> int -> int -> int -> 'a array -> int -> int -> ('a, 'd) custom -> 'd -> bool -> bool -> unit;
    item_string : 'd -> 'a -> string;
  }

type 'a command =
  Select of int
| SetList of ('a array * bool IntMap.t * int) (* sel *)
| SetPos of int
| QuerySize

type 'a notification =
  OnSelect of 'a array * int
| OnCheck of 'a array * int * bool
| OnAction of 'a array * int * bool
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
    mutable chkmap : bool IntMap.t;
    mutable inv : event option;

	mutable checkbox_size : float;
	mutable top_margin : float;
	mutable left_margin : float;
	mutable box_line_width : float;
	mutable left_margin_text : float;
  }

let path_check = [|
    (0.0, 0.3746393915552059);
	(0.05546813532651455, 0.3208759506949908);
	(0.1198531340152111, 0.2648832939942303);
	(0.1778127458693942, 0.2239706268030422);
	(0.2428533962758983, 0.3010752688172043);
	(0.3273013375295044, 0.3894571203776554);
	(0.3971938106477839, 0.4665617623918175);
	(0.5491738788355626, 0.3075006556517178);
	(0.6816155258326777, 0.1746656176239182);
	(0.8240230789404668, 0.0);
	(0.8881458169420404, 0.05153422501966955);
	(0.9471544715447154, 0.1039863624442696);
	(1.0, 0.1660110149488592);
	(0.7789142407553108, 0.3919485969053239);
	(0.5967741935483871, 0.5966430632048256);
	(0.3899816417519014, 0.8291371623393653);
	(0.2706530291109363, 0.6712562286913192);
	(0.07867820613690008, 0.454628901127721);
	(0.0, 0.3746393915552059);
  |]

let draw_icon_check x y size =
  push_translate_scale x y size;
  fill_bezier path_check;
  pop_translate_scale ()

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
  let ih = float_of_int ud.item_height in
  let checkbox_size = ih *. 0.7 in
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
      chkmap = IntMap.empty;

	  checkbox_size = checkbox_size;
	  top_margin = 0.5 *. (ih -. checkbox_size);
	  left_margin = 0.15 *. ih;
	  box_line_width = 0.5 *. (o_o.ppi /. 72.0);
	  left_margin_text = 1.15 *. ih;
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
    | SetList (cs, chkmap, sel) ->
       cc.cs <- cs;
       cc.chkmap <- chkmap;
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
    request Activate;
    let i = y / cc.ud.item_height in
    let index = cc.pos + i in
    if i >= 0 && index < Array.length cc.cs then
      let yi = float_of_int (y - i * ud.item_height) in
      let xf = float_of_int x in
      (if xf >= cc.left_margin && xf < cc.left_margin +. cc.checkbox_size &&
            yi >= cc.top_margin && yi < cc.top_margin +. cc.checkbox_size then
         (* checkbox *)
         let b = get_check_state cc.chkmap index in
         cc.chkmap <- IntMap.add index (not b) cc.chkmap;
         notify (OnCheck (cc.cs, index, not b)));
      (if index = cc.sel then (
         if is_double_clicked state then (
           let b = get_check_state cc.chkmap index in
           notify (OnAction (cc.cs, index, b));
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
           Listbox.interval_fold 0 (Array.length cc.cs) ""
             (fun s i ->
               let item = Array.get cc.cs i in
               s ^ "\n" ^ (ud.item_string dc item))
         in
         clipboard_set_text s;
         process ()
    else
      process ()

  and paint x y w h =
    let x_ofs = int_of_float (cc.left_margin *. 2.0 +. cc.checkbox_size) in
    let rec loop i y =
      if i < Array.length cc.cs && y < cc.height then
        let b_check = get_check_state cc.chkmap i in
        ud.draw x_ofs y cc.width ud.item_height cc.cs i cc.sel ud dc cc.act b_check;
        set_color o_o.color_fore;
        set_line_width cc.box_line_width;
        draw_rect
          cc.left_margin
          ((flo y) +. cc.top_margin)
          cc.checkbox_size cc.checkbox_size;
        (if b_check then draw_check 0 y);
        loop (i + 1) (y + ud.item_height)
    in
    set_color o_o.color_back;
    fill_rect 0.0 0.0 cc.fWidth cc.fHeight;
    set_font o_o.font;
    loop cc.pos 0;
    send pch (wch, PaintAck) process

  and draw_check x y =
    set_color o_o.color_fore;
    draw_icon_check
      (cc.left_margin +. (flo x))
      (cc.top_margin +. (flo y))
      cc.checkbox_size

  and request msg =
    Queue.add (sendEvt pch (wch, msg) pque_drop) pque

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

  and get_check_state chkmap index =
    match IntMap.find_opt index chkmap with
      None -> false
    | Some b -> b

  in process ()
