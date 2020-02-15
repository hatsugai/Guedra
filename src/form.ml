open Csp
open Guedra

type client = {
    cwch : win_msg chan;
    mutable rb : int rect;
  }

type context = {
    mutable width : int;
    mutable height : int;
    mutable inv : event option;
    mutable cap : client option;
    mutable act : bool;
    mutable cact : win_msg chan option;
  }

let rec find_client cs wch =
  match cs with
    [] -> error "form find_client"
  | c::cs' ->
     if c.cwch == wch then
       c
     else
       find_client cs' wch

let rec hittest cs x y =
  match cs with
    [] -> None
  | c::cs' ->
     if pt_in_rect x y c.rb then
       Some c
     else
       hittest cs' x y

let init wch pch rch cs =
  let cs = List.map (fun (cwch, r) -> { cwch = cwch; rb = r; }) cs in
  let pque = Queue.create () in
  let nque = Queue.create () in
  let cc = {
      width = 0;
      height = 0;
      inv = None;
      cap = None;
      act = false;
      cact = None;
    }
  in

  let rec process () =
    let event_list =
      [ recvEvt wch always win_msg;
        recvEvt rch always win_cmd ]
    in select_que3 event_list cc.inv pque nque

  and win_msg msg =
    match msg with
      Paint (x, y, w, h) ->
       set_color o_o.color_back;
       fill_rect 0.0 0.0
         (float_of_int cc.width) (float_of_int cc.height);
       let rec loop xs =
         match xs with
           [] ->
            send pch (wch, PaintAck) process
         | c::xs' ->
            push_translate (flo c.rb.x) (flo c.rb.y);
            push_clip 0 0 c.rb.w c.rb.h;
            send c.cwch
              (Paint (x - c.rb.x, y - c.rb.y, w, h))
              (fun () ->
                recv rch (guard_paintack c.cwch)
                  (fun _ ->
                    pop_clip ();
                    pop_translate ();
                    loop xs'))
       in loop cs

    | WinSize (w, h) ->
       cc.width <- w;
       cc.height <- h;
       process ()

    | MouseDown (x, y, state, button) ->
       (match hittest cs x y with
          None -> process ()
        | Some c ->
           cc.cap <- Some c;
           send c.cwch
             (MouseDown (x - c.rb.x, y - c.rb.y, state, button))
             process)

    | MouseUp (x, y, state, button) ->
       (match cc.cap with
          None -> process ()
        | Some c ->
           cc.cap <- None;
           send c.cwch
             (MouseUp (x - c.rb.x, y - c.rb.y, state, button))
             process)

    | MouseMove (x, y, state) ->
       (match cc.cap with
          None ->
           (match hittest cs x y with
              None -> process ()
            | Some c ->
               send c.cwch
                 (MouseMove (x - c.rb.x, y - c.rb.y, state))
                 process)
        | Some c ->
           send c.cwch
             (MouseMove (x - c.rb.x, y - c.rb.y, state))
             process)

    | MouseWheel (x, y, state, direction) ->
       (match hittest cs x y with
          None -> process ()
        | Some c ->
           send c.cwch
             (MouseWheel (x - c.rb.x, y - c.rb.y, state, direction))
             process)

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

  and win_cmd (ch, msg) =
    let c = find_client cs ch in
    match msg with
      Invalidate (x, y, w, h) ->
       (match cc.inv with
          None ->
           let e =
             sendEvt pch
               (wch, (Invalidate (x + c.rb.x, y + c.rb.y, w, h)))
               clear_inv
           in
           cc.inv <- Some e;
           process ()
        | Some e -> (* ### *)
           let e =
             sendEvt pch
               (wch, (Invalidate (0, 0, cc.width, cc.height)))
               clear_inv
           in
           cc.inv <- Some e;
           process ())
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

    and clear_inv () =
      cc.inv <- None;
      process ()

    and pque_drop () = let _ = Queue.take pque in process ()

    in
    (* send initial size *)
    let rec loop xs =
      match xs with
        [] -> process ()
      | c::xs' ->
         send c.cwch (WinSize (c.rb.w, c.rb.h))
           (fun () -> loop xs')
    in
    loop cs
