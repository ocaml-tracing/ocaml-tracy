open Trace_core

let spf = Printf.sprintf

open struct
  type Trace_core.span += Span_tracy of int
  type st = unit

  let name_thread = Tracy_client.name_thread

  let process_data sp (d : string * Trace_core.user_data) =
    let k, v = d in
    let msg =
      match v with
      | `String s -> spf "%s: %s\n" k s
      | `Int i -> spf "%s: %d\n" k i
      | `Bool b -> spf "%s: %b\n" k b
      | `None -> spf "%s\n" k
      | `Float f -> spf "%f\n" f
    in
    Tracy_client.add_text sp msg

  let enter_span (_ : st) ~__FUNCTION__ ~__FILE__ ~__LINE__ ~params:_ ~data
      ~parent:_ name : span =
    let sp = Tracy_client.enter ?__FUNCTION__ ~__FILE__ ~__LINE__ name in
    if data <> [] then List.iter (process_data sp) data;
    Span_tracy sp

  let exit_span (_ : st) (sp : span) : unit =
    match sp with
    | Span_tracy sp -> Tracy_client.exit sp
    | _ -> ()

  let message (_ : st) ~params:_ ~data:_ ~span:_ msg : unit =
    Tracy_client.message msg

  let counter_float _ ~params:_ ~data:_ name n : unit = Tracy_client.plot name n

  let counter_int st ~params ~data name n : unit =
    counter_float st ~params ~data name (float_of_int n)

  let add_data_to_span _ _ _ = ()

  let extension (_ : st) ev =
    match ev with
    | Trace_core.Core_ext.Extension_set_thread_name name -> name_thread name
    | _ -> ()

  let callbacks : unit Collector.Callbacks.t =
    Collector.Callbacks.make ~enter_span ~exit_span ~add_data_to_span ~message
      ~counter_int ~counter_float ~extension ()
end

let collector () : Collector.t = Collector.C_some ((), callbacks)
let setup () = Trace_core.setup_collector @@ collector ()
