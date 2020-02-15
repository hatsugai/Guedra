open Csp
open Guedra

type command =
  SetIndex of int
| SetRange of int * int         (* range, pos *)

type notification =
  Pos of int

type context = {
    mutable width : int;
    mutable height : int;
    mutable fWidth : float;
    mutable fHeight : float;
    mutable range : int;
    mutable page : int;
    mutable index : int;        (* 0 <= index <= range *)
    mutable pos : int;          (* thumb coordinate *)
    mutable displacement : int;
    mutable capture : bool;
    mutable inv : event option;
  }

let make_cc range0 page0 index0 =
  {
      width = 0;
      height = 0;
      fWidth = 0.0;
      fHeight = 0.0;
      range = range0;
      page = page0;
      index = index0;
      displacement = 0;
      pos = 0;
      capture = false;
      inv = None;
    }

let calc_pos w_or_h range index =
  if w_or_h <= o_o.scroll_thumb_size || range = 0 then
    0
  else if index = range then    (* 0 <= index <= range *)
    w_or_h - o_o.scroll_thumb_size
  else
    ((w_or_h - o_o.scroll_thumb_size) * index) / range

let calc_index w_or_h range pos =
  if w_or_h <= o_o.scroll_thumb_size || range = 0 then
    0
  else
    max 0 (min range ((pos * range) / (w_or_h - o_o.scroll_thumb_size)))
