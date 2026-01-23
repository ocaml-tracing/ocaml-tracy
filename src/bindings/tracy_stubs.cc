
#include <algorithm>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <cstdint>
#include <limits>
#include <stdbool.h>
#include <stdint.h>

// needed for macros to do anything
#define TRACY_ENABLE 1

#include "tracy/public/tracy/TracyC.h"

extern "C" {

#define INT_OF_CTX(ctx) ((((uint64_t)ctx.id) << 1) | ((uint64_t)ctx.active))
#define CTX_OF_INT(i)                                                          \
  { .id = ((uint32_t)(i >> 1)), .active = ((i & 1) != 0), }

// return the unique uint32 for this zone
CAMLprim value ml_tracy_enter(value file, value fun, value line, value name,
                              value depth) {
  CAMLparam5(file, fun, line, name, depth);

  // declare a srcloc
  uint32_t c_line = (uint32_t)Int_val(line);

  char const *c_file = String_val(file);
  size_t c_file_len = caml_string_length(file);

  char const *c_fun = String_val(fun);
  size_t c_fun_len = caml_string_length(fun);

  char const *c_name = String_val(name);
  size_t c_name_len = caml_string_length(name);

  uint64_t srcloc = ___tracy_alloc_srcloc_name(
      c_line, c_file, c_file_len, c_fun, c_fun_len, c_name, c_name_len, 0);

  const uint32_t max_stack_depth = 62;
  uint32_t c_depth = (uint32_t)Int_val(depth);
  c_depth = std::min(c_depth, max_stack_depth);

  // enter zone
  TracyCZoneCtx ctx =
      c_depth > 0
          ? ___tracy_emit_zone_begin_alloc_callstack(srcloc, c_depth, true)
          : ___tracy_emit_zone_begin_alloc(srcloc, true);

  uint64_t res = INT_OF_CTX(ctx);

  CAMLreturn(Val_int((int)res));
}

// exit using the value returned by `ml_tracy_enter`
CAMLprim value ml_tracy_exit(value span) {
  CAMLparam1(span);

  uint64_t bundle = Int_val(span);
  TracyCZoneCtx ctx = CTX_OF_INT(bundle);

  ___tracy_emit_zone_end(ctx);

  CAMLreturn(Val_unit);
}

CAMLprim value ml_tracy_span_color(value span, value color) {
  CAMLparam2(span, color);

  uint64_t bundle = Int_val(span);
  TracyCZoneCtx ctx = CTX_OF_INT(bundle);

  uint64_t c_color = Int_val(color);

  ___tracy_emit_zone_color(ctx, c_color);

  CAMLreturn(Val_unit);
}

CAMLprim value ml_tracy_span_value(value span, value v) {
  CAMLparam2(span, v);

  uint64_t bundle = Int_val(span);
  TracyCZoneCtx ctx = CTX_OF_INT(bundle);

  uint64_t c_val = Int64_val(v);

  ___tracy_emit_zone_value(ctx, c_val);

  CAMLreturn(Val_unit);
}

// tracy has internal size limits on messages, it seems
const static size_t max_message_len =
    std::numeric_limits<std::uint16_t>::max() - 1;

CAMLprim value ml_tracy_span_text(value span, value txt) {
  CAMLparam2(span, txt);

  uint64_t bundle = Int_val(span);
  TracyCZoneCtx ctx = CTX_OF_INT(bundle);

  const char *c_text = String_val(txt);
  size_t c_text_len = caml_string_length(txt);
  c_text_len = std::min(c_text_len, max_message_len);

  ___tracy_emit_zone_text(ctx, c_text, c_text_len);

  CAMLreturn(Val_unit);
}

CAMLprim value ml_tracy_name_thread(value name) {
  CAMLparam1(name);

  char const *c_name = String_val(name);
  TracyCSetThreadName(c_name);

  CAMLreturn(Val_unit);
}

CAMLprim value ml_tracy_msg(value name) {
  CAMLparam1(name);

  char const *c_name = String_val(name);

  size_t c_len = caml_string_length(name);
  c_len = std::min(c_len, max_message_len);

  TracyCMessage(c_name, c_len);

  CAMLreturn(Val_unit);
}

CAMLprim value ml_tracy_app_info(value name) {
  CAMLparam1(name);

  char const *c_name = String_val(name);
  size_t c_len = caml_string_length(name);

  TracyCAppInfo(c_name, c_len);

  CAMLreturn(Val_unit);
}

CAMLprim value ml_tracy_plot(value name, value x) {
  CAMLparam2(name, x);

  char const *c_name = String_val(name);
  // size_t c_len = caml_string_length(name);

  double c_x = Double_val(x);

  TracyCPlot(c_name, c_x);

  CAMLreturn(Val_unit);
}

CAMLprim value ml_tracy_frame_enter(value name) {
  char const *c_name = String_val(name);
  TracyCFrameMarkStart(c_name);
  return Val_unit;
}

CAMLprim value ml_tracy_frame_exit(value name) {
  char const *c_name = String_val(name);
  TracyCFrameMarkEnd(c_name);
  return Val_unit;
}


} // extern "C"

