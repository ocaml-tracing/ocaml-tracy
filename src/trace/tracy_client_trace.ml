type explicit_span = Trace.explicit_span

let spf = Printf.sprintf

module C () : Trace.Collector.S = struct
  let name_thread = Tracy_client.name_thread
  let name_process _ = ()

  let process_data sp (d : string * Trace.user_data) =
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

  let enter_span ~__FUNCTION__ ~__FILE__ ~__LINE__ ~data name : Trace.span =
    let sp = Tracy_client.enter ?__FUNCTION__ ~__FILE__ ~__LINE__ name in
    if data <> [] then List.iter (process_data sp) data;
    Int64.of_int sp

  let exit_span (sp : Trace.span) : unit = Tracy_client.exit (Int64.to_int sp)

  let with_span ~__FUNCTION__ ~__FILE__ ~__LINE__ ~data name f =
    let sp = enter_span ~__FUNCTION__ ~__FILE__ ~__LINE__ ~data name in
    let finally () = exit_span sp in
    Fun.protect ~finally (fun () -> f sp)

  let message ?span:_ ~data:_ msg : unit = Tracy_client.message msg
  let counter_float ~data:_ name n : unit = Tracy_client.plot name n

  let counter_int ~data name n : unit =
    counter_float ~data name (float_of_int n)

  let shutdown () = ()
  let add_data_to_span _ _ = ()
  let add_data_to_manual_span _ _ = ()
  let enter_context name = Tracy_client.enter_frame name
  let exit_context name = Tracy_client.exit_frame name

  let enter_manual_span ~parent:_ ~flavor:_ ~__FUNCTION__:_ ~__FILE__:_
      ~__LINE__:_ ~data:_ _name : explicit_span =
    Trace.Collector.dummy_explicit_span

  let exit_manual_span _es : unit = ()
  let extension_event _ = ()
end

let collector () : Trace.collector =
  let module C = C () in
  (module C)

let setup () = Trace.setup_collector @@ collector ()
