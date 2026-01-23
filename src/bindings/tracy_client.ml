type span = int
(* technically, [span] is [(bool,uint32)], but since tracy required 64 bits,
   it always fits *)

external _tracy_enter :
  file:string -> fun_:string -> line:int -> name:string -> depth:int -> span
  = "ml_tracy_enter"
[@@noalloc]

external _tracy_exit : span -> unit = "ml_tracy_exit" [@@noalloc]
external _tracy_name_thread : string -> unit = "ml_tracy_name_thread"
external _tracy_msg : string -> unit = "ml_tracy_msg" [@@noalloc]
external _tracy_plot : string -> float -> unit = "ml_tracy_plot" [@@noalloc]
(* TODO external _tracy_set_plot_unit : string -> float -> unit = "ml_tracy_plot" *)

external _tracy_app_info : string -> unit = "ml_tracy_app_info"

external _tracy_span_text : span -> string -> unit = "ml_tracy_span_text"
[@@noalloc]

external _tracy_span_value : span -> int64 -> unit = "ml_tracy_span_value"
[@@noalloc]

external _tracy_span_color : span -> int -> unit = "ml_tracy_span_color"
[@@noalloc]

external _tracy_frame_enter : string -> unit = "ml_tracy_frame_enter"
  [@@noalloc]

external _tracy_frame_exit : string -> unit = "ml_tracy_frame_exit" [@@noalloc]

let enter ?cs_depth ~__FILE__:file ~__LINE__:line ?(__FUNCTION__ = "<fun>") name
    : span =
  let depth =
    match cs_depth with
    | None -> 0
    | Some i -> i
  in
  _tracy_enter ~file ~fun_:__FUNCTION__ ~line ~name ~depth

(* TODO: in enter/tracy_enter,
   use ___tracy_emit_zone_value/color/text for bonus items *)

let exit = _tracy_exit
let name_thread = _tracy_name_thread
let message = _tracy_msg
let plot = _tracy_plot
let set_color = _tracy_span_color
let add_value = _tracy_span_value
let add_text = _tracy_span_text
let set_app_info = _tracy_app_info
let message_f k = k (fun fmt -> Format.kasprintf message fmt)
let add_text_f sp k = k (fun fmt -> Format.kasprintf (add_text sp) fmt)
let enter_frame = _tracy_frame_enter
let exit_frame = _tracy_frame_exit

let[@inline] with_frame name f =
  enter_frame name;
  try
    let x = f () in
    exit_frame name;
    x
  with e ->
    exit_frame name;
    raise e

let[@inline] with_ ?cs_depth ~__FILE__ ~__LINE__ ?__FUNCTION__ name f =
  let _sp = enter ?cs_depth ~__FILE__ ~__LINE__ ?__FUNCTION__ name in
  try
    let x = f _sp in
    exit _sp;
    x
  with e ->
    exit _sp;
    raise e
