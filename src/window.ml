open Csp
open Guedra

type window = {
    mutable width : int;
    mutable height : int;
  }

let path_check = [|
    (0.0,      0.374639);
	(0.055468, 0.320875);
	(0.119853, 0.264883);
	(0.177812, 0.223970);
	(0.242853, 0.301075);
	(0.327301, 0.389457);
	(0.397193, 0.466561);
	(0.549173, 0.307500);
	(0.681615, 0.174665);
	(0.824023, 0.0);
	(0.888145, 0.051534);
	(0.947154, 0.103986);
	(1.0,      0.166011);
	(0.778914, 0.391948);
	(0.596774, 0.596643);
	(0.389981, 0.829137);
	(0.270653, 0.671256);
	(0.078678, 0.454628);
	(0.0,      0.374639);
  |]

let init wch pch =
  let pque = Queue.create () in
  let cc = { width = 0; height = 0 } in

  let rec process () =
    let ev = recvEvt wch always win_msg in
    select_que [ev] pque

  and win_msg msg =
    match msg with
      Paint (x, y, w, h) ->
       paint x y w h
    | WinSize (w, h) ->
       prn (Printf.sprintf "size %d %d\n" w h);
       cc.width <- w;
       cc.height <- h;
       process ()
    | MouseEnter ->
       prn "MouseEnter\n";
       process ()
    | MouseLeave ->
       prn "MouseLeave\n";
       process ()
    | MouseDown (x, y, state, button) ->
       prn (Printf.sprintf "MouseDown %d %d %x %d\n" x y state button);
       request Activate;
       process ()
    | MouseUp (x, y, state, button) ->
       prn (Printf.sprintf "MouseUp %d %d\n" x y);
       process ()
    | MouseMove (x, y, state) ->
       (* prn (Printf.sprintf "MouseMove %d %d\n" x y); *)
       process ()
    | MouseWheel (x, y, state, direction) ->
       prn (Printf.sprintf "MouseWheel %d\n" direction);
       process ()
    | KeyDown (key, state) ->
       prn (Printf.sprintf "%d KeyDown %d %x\n" (chid wch) key state);
       process ()
    | KeyChar code ->
       prn (Printf.sprintf "%d KeyChar %x\n" (chid wch) code);
       process ()
    | _ -> process ()

  and paint x y w h =
    prn "wndow paint\n";
    set_color o_o.color_back;
    fill_rect 0.0 0.0 (float_of_int cc.width) (float_of_int cc.height);

    set_line_width 6.0;
    set_color (color 1.0 0.0 0.0);
    draw_line 0.0 0.0 (float_of_int cc.width) (float_of_int cc.height);
    set_color (color 0.0 0.0 1.0);
    draw_line (float_of_int cc.width) 0.0 0.0 (float_of_int cc.height);

    set_color (color 0.0 0.5 0.0);
    set_font o_o.font;
    draw_text 50.0 50.0 "Hello, World";

    set_color (color 0.0 0.5 0.0);
    draw_rect 200.0 150.0 100.0 50.0;
    set_color (color 0.0 0.2 1.0);
    fill_rect 200.0 250.0 100.0 50.0;

    set_color (color 0.0 0.5 0.0);
    push_translate_scale 50.0 150.0 50.0;
    fill_bezier path_check;
    pop_translate_scale ();
    set_color (color 0.0 0.2 1.0);
    push_translate_scale 50.0 250.0 50.0;
    fill_bezier path_check;
    pop_translate_scale ();

    prn "window paint done\n";
    send pch (wch, PaintAck) process

  and request msg =
    Queue.add (sendEvt pch (wch, msg) pque_drop) pque

  and pque_drop () = let _ = Queue.take pque in process ()

  in process ()
