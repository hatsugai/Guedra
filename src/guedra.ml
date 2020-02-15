open Csp

exception Error of string

let error s = raise (Error s)

type font
type color = { red : float; green : float; blue : float; }
type font_style = FONT_STYLE_NORMAL | FONT_STYLE_ITALIC | FONT_STYLE_OBLIQUE
type font_weight = FONT_WEIGHT_NORMAL | FONT_WEIGHT_BOLD
type path_fill = PATH_HOLLOW | PATH_FILL
type path_close = PATH_OPEN | PATH_CLOSE
type text_extents = {
    width : float;
    height : float;
    baseline : float;
  }

let mouseButtonLeft             = 1
let mouseButtonRight            = 2
let mouseButtonMiddle           = 4
let mouseButtonStateDoubleClick = 8
let keyStateShift               = 16
let keyStateControl             = 32
let keyStateAlt                 = 64

let keyStateMask = keyStateShift lor keyStateControl lor keyStateAlt

let keyBackSpace = 65288
let keyTab       = 65289
let keyReturn    = 65293
let keyEscape    = 65307
let keyDelete    = 65535
let keyHome      = 65360
let keyLeft      = 65361
let keyUp        = 65362
let keyRight     = 65363
let keyDown      = 65364
let keyPageUp    = 65365
let keyPageDown  = 65366
let keyEnd       = 65367
let keyInsert    = 65379
let keyF1        = 65470
let keyF2        = 65471
let keyF3        = 65472
let keyF4        = 65473
let keyF5        = 65474
let keyF6        = 65475
let keyF7        = 65476
let keyF8        = 65477
let keyF9        = 65478
let keyF10       = 65479
let keyF11       = 65480
let keyF12       = 65481
let keyShift     = 65505
let keyControl   = 65507
let keyMeta      = 65511
let keyAlt       = 65513

type mouseCursor =
  MouseCursorProgress
| MouseCursorArrow
| MouseCursorCross
| MouseCursorHand
| MouseCursorHelp
| MouseCursorText
| MouseCursorNo
| MouseCursorMove
| MouseCursorNWSE
| MouseCursorNESW
| MouseCursorNS
| MouseCursorWE
| MouseCursorWait

external prn : string -> unit =
  "guedra_prn"
external create_window : string -> int -> int -> int -> int -> int =
  "guedra_create_window"
external invalidate : int -> int -> int -> int -> int -> unit =
  "guedra_invalidate"
external set_color : color -> unit =
  "guedra_set_color"
external set_line_width : float -> unit =
  "guedra_set_line_width"
external draw_line : float -> float -> float -> float -> unit =
  "guedra_draw_line"
external draw_rect : float -> float -> float -> float -> unit =
  "guedra_draw_rect"
external fill_rect : float -> float -> float -> float -> unit =
  "guedra_fill_rect"
external draw_text : float -> float -> string -> unit =
  "guedra_draw_text"
external push_clip : int -> int -> int -> int -> unit =
  "push_clip"
external pop_clip : unit -> unit =
  "pop_clip"
external push_translate : float -> float -> unit =
  "guedra_push_translate"
external pop_translate : unit -> unit =
  "guedra_pop_transform"
external push_translate_scale : float -> float -> float -> unit =
  "guedra_push_translate_scale"
external pop_translate_scale : unit -> unit =
  "guedra_pop_transform"
external text_extents : string -> text_extents =
  "guedra_text_extents"
external make_font : string -> font_style -> font_weight -> float -> font =
  "make_font"
external delete_font : font -> unit =
  "delete_font"
external set_font : font -> unit =
  "set_font"
external set_dash : float array -> float -> unit =
  "guedra_set_dash"
external set_dash_solid : unit -> unit =
  "guedra_set_dash_solid"
external set_dash_symmetric : float -> float -> unit =
  "guedra_set_dash_symmetric"
external fill_bezier : (float * float) array -> unit =
  "guedra_fill_bezier"
external create_svg_surface : string -> float -> float -> unit =
  "imp_create_svg_surface"
external create_pdf_surface : string -> float -> float -> unit =
  "imp_create_pdf_surface"
external delete_surface : unit -> unit =
  "imp_delete_surface"
external clipboard_set_text : string -> unit =
  "imp_clipboard_set_text"
external guedra_quit : unit -> unit =
  "imp_guedra_quit"

let is_double_clicked mouse_event_state =
  (mouse_event_state land mouseButtonStateDoubleClick) <> 0

let flo n = float_of_int n

let select_que event_list que =
  let event_list =
    if Queue.is_empty que then
      event_list
    else
      (Queue.top que)::event_list
  in
  select event_list

let select_que2 event_list que0 que1 =
  let event_list =
    if Queue.is_empty que0 then
      event_list
    else
      (Queue.top que0)::event_list
  in
  let event_list =
    if Queue.is_empty que1 then
      event_list
    else
      (Queue.top que1)::event_list
  in
  select event_list

let select_que3 event_list inv pque nque =
  let event_list =
    if Queue.is_empty pque then
      match inv with
        None -> event_list
      | Some e -> e::event_list
    else
      (Queue.top pque)::event_list
  in
  let event_list =
    if Queue.is_empty nque then
      event_list
    else
      (Queue.top nque)::event_list
  in
  select event_list

let always x = true

let color r g b = { red = r; green = g; blue = b; }
type 'a point = { x : 'a; y : 'a; }
type 'n rect = { x : 'n; y : 'n; w : 'n; h : 'n; }
let rect x y w h = { x = x; y = y; w = w; h = h; }
let pt_in_rect x y r = x >= r.x && x < r.x + r.w && y >= r.y && y < r.y + r.h

type win_msg =
  EndSession
| Timer
| WinSize of int * int
| WinClose
| Paint of int * int * int * int
| MouseEnter
| MouseLeave
| MouseDown of int * int * int * int
| MouseUp of int * int * int * int
| MouseMove of int * int * int
| MouseWheel of int * int * int * int
| KeyDown of int * int
| KeyUp of int * int
| KeyChar of int
| Active of bool
| Scroll of int * int * int * int * int
| Scale of float

type win_req =
  PaintAck
| Activate
| Invalidate of int * int * int * int

type preferences = {
    (* color *)
    mutable color_fore : color;
    mutable color_back : color;
    mutable color_handle : color;

    (* metrics *)
    mutable ppi : float;
    mutable pt : float;
    mutable line_width : float;
    mutable dash : float;
    mutable mouse_dist_threshold : float;
    mutable scroll_thumb_size : int;
    mutable handle_size : float;

    (* font *)
    mutable font_name : string;
    mutable font_size : float;
    mutable font : font;
    mutable funit : float;

    (* text *)
    mutable left_margin : float;
    mutable top_margin : float;
    mutable right_margin : float;
    mutable bottom_margin : float;
    }

let color_black = color 0.0 0.0 0.0
let color_white = color 1.0 1.0 1.0

(* dummy for init *)
let font_name = "monospace"
let font_size = 10.0

let o_o = {
    (* color *)
    color_fore = color_white;
    color_back = color 0.2 0.2 0.2;
    color_handle = color 0.0 1.0 0.5;

    (* metrics *)
    ppi = 96.0;
    pt = 0.0;
    line_width = 0.0;
    dash = 0.0;
    mouse_dist_threshold = 0.0;
    scroll_thumb_size = 0;
    handle_size = 0.0;

    (* font *)
    font_name = font_name;
    font_size = font_size;
    font = make_font font_name FONT_STYLE_NORMAL FONT_WEIGHT_NORMAL font_size;
    funit = 1.0;

    (* text : funit *)
    left_margin   = 0.0;
    right_margin  = 0.0;
    top_margin    = 0.0;
    bottom_margin = 0.0;
  }

type window_process = {
    win_id : int;
    wch : win_msg chan;
    shortcut_control : (int, win_msg chan) Hashtbl.t;
    shortcut_alt : (int, win_msg chan) Hashtbl.t;
  }

let guard_paintack ch =
  (fun (ch', msg) ->
    ch == ch' && msg = PaintAck)

let dch = make_chan ()
let rch = ((make_chan ()) : (win_msg chan * win_req) chan)
let win_id_to_wp = Hashtbl.create 100
let chid_to_wp = Hashtbl.create 100

let client_init = ref None

let reg_client
      ?(ppi = 96.0) ?(font_size = 10.0) ?(font_name = "monospace")
      init =
  o_o.ppi <- ppi;
  o_o.font_size <- font_size;
  o_o.font_name <- font_name;
  client_init := Some init

let create_toplevel_window title x y w h =
  let win_id = create_window title x y w h  in
  let wch = make_chan () in
  let wp = {
      win_id = win_id;
      wch = wch;
      shortcut_control = Hashtbl.create 7;
      shortcut_alt = Hashtbl.create 7;
    }
  in
  Hashtbl.add win_id_to_wp win_id wp;
  Hashtbl.add chid_to_wp (chid wch) wp;
  (wch, rch)

let register_shortcut_control wch_toplevel wch_receiver key =
  let wp = Hashtbl.find chid_to_wp (chid wch_toplevel) in
  Hashtbl.replace wp.shortcut_control key wch_receiver

let register_shortcut_alt wch_toplevel wch_receiver key =
  let wp = Hashtbl.find chid_to_wp (chid wch_toplevel) in
  Hashtbl.replace wp.shortcut_alt key wch_receiver

let init_resources () =
  o_o.pt <- o_o.ppi /. 72.0;
  let font =
    make_font o_o.font_name FONT_STYLE_NORMAL FONT_WEIGHT_NORMAL
      (o_o.font_size *. o_o.pt)
  in
  delete_font o_o.font;
  o_o.font <- font;
  set_font font;
  let extents = text_extents "0AMWgy()[]{}|@#$,." in
  o_o.funit <- extents.height;
  (* text *)
  o_o.left_margin   <- o_o.funit *. 0.2;
  o_o.right_margin  <- o_o.funit *. 0.3;
  o_o.top_margin    <- o_o.funit *. 0.15;
  o_o.bottom_margin <- o_o.funit *. 0.15;
  (* metrics *)
  o_o.line_width <- 0.75 *. o_o.pt;
  (if o_o.line_width < 1.0 then
     o_o.line_width <- 1.0);
  o_o.dash <- o_o.funit *. 0.2;
  o_o.mouse_dist_threshold <- o_o.funit *. 0.6;
  o_o.handle_size <- o_o.funit *. 0.3;
  o_o.scroll_thumb_size <- int_of_float (o_o.funit *. 0.6)

let rec winsys_broker () =
  select [
      recvEvt dch always
        (fun (win_id, notif) ->
          let wp = Hashtbl.find win_id_to_wp win_id in
          match notif with
            Paint (x, y, w, h) ->
             send wp.wch notif
               (fun () ->
                 recv rch (guard_paintack wp.wch)
                   (fun _ -> winsys_broker ()))
          | KeyDown (key, state) ->
             if (state land keyStateMask) = keyStateControl then
               match Hashtbl.find_opt wp.shortcut_control key with
                 None -> send wp.wch notif winsys_broker
               | Some wch -> send wch notif winsys_broker
             else if (state land keyStateMask) = keyStateAlt then
               match Hashtbl.find_opt wp.shortcut_alt key with
                 None -> send wp.wch notif winsys_broker
               | Some wch -> send wch notif winsys_broker
             else
               send wp.wch notif winsys_broker
          | _ ->
             send wp.wch notif winsys_broker);
      recvEvt rch always
        (fun (ch, msg) ->
          let wp = Hashtbl.find chid_to_wp (chid ch) in
          match msg with
            PaintAck ->
             winsys_broker ()
          | Activate ->
             send ch (Active true) winsys_broker
          | Invalidate (x, y, w, h) ->
             invalidate wp.win_id x y w h;
             winsys_broker ())
    ]

let start () =
  match !client_init with
    None -> error "client not initialized"
  | Some thunk ->
     par [winsys_broker; thunk]

let init () =
  init_resources ();
  init_csp start

let drive win_id msg =
  inject dch (win_id, msg)

let _ = Callback.register "guedra init" init
let _ = Callback.register "guedra drive" drive
