#define CAML_NAME_SPACE
#include <stdio.h>
#include <stdbool.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include "win.h"

static const value *proc;
bool b_calling;

void guedra_init(void)
{
    const value *init = caml_named_value("guedra init");
    proc = caml_named_value("guedra drive");
    b_calling = true;
    {
        caml_callback(*init, Val_unit);
    }
    b_calling = false;
}

void callback(int win_id, value msg)
{
    if (!b_calling) {
        b_calling = true;
        {
            caml_callback2(*proc, Val_int(win_id), msg);
        }
        b_calling = false;
    } else {
        fprintf(stderr, "tried to reenter\n");
        fflush(stderr);
    }
}

void notify_end_session(void)
{
    callback(0, Val_int(Tag_EndSession));
}

void notify_timer(void)
{
    callback(0, Val_int(Tag_Timer));
}

void notify_win_size(unsigned win_id, int w, int h)
{
    value msg = caml_alloc(2, Tag_WinSize);
    Store_field(msg, 0, Val_int(w));
    Store_field(msg, 1, Val_int(h));
    callback(win_id, msg);
}

void notify_win_close(unsigned win_id)
{
    callback(0, Val_int(Tag_WinClose));
}

void notify_paint(unsigned win_id, int x, int y, int w, int h)
{
    value msg = caml_alloc(4, Tag_Paint);
    Store_field(msg, 0, Val_int(x));
    Store_field(msg, 1, Val_int(y));
    Store_field(msg, 2, Val_int(w));
    Store_field(msg, 3, Val_int(h));
    callback(win_id, msg);
}

void notify_mouse_enter(unsigned win_id)
{
    callback(0, Val_int(Tag_MouseEnter));
}

void notify_mouse_leave(unsigned win_id)
{
    callback(0, Val_int(Tag_MouseLeave));
}

void notify_mouse_down(unsigned win_id, int x, int y, unsigned state, unsigned button)
{
    value msg = caml_alloc(4, Tag_MouseDown);
    Store_field(msg, 0, Val_int(x));
    Store_field(msg, 1, Val_int(y));
    Store_field(msg, 2, Val_long(state));
    Store_field(msg, 3, Val_long(button));
    callback(win_id, msg);
}

void notify_mouse_up(unsigned win_id, int x, int y, unsigned state, unsigned button)
{
    value msg = caml_alloc(4, Tag_MouseUp);
    Store_field(msg, 0, Val_int(x));
    Store_field(msg, 1, Val_int(y));
    Store_field(msg, 2, Val_long(state));
    Store_field(msg, 3, Val_long(button));
    callback(win_id, msg);
}

void notify_mouse_move(unsigned win_id, int x, int y, unsigned state)
{
    value msg = caml_alloc(3, Tag_MouseMove);
    Store_field(msg, 0, Val_int(x));
    Store_field(msg, 1, Val_int(y));
    Store_field(msg, 2, Val_long(state));
    callback(win_id, msg);
}

void notify_mouse_wheel(unsigned win_id, int x, int y, unsigned state, int direction)
{
    value msg = caml_alloc(4, Tag_MouseWheel);
    Store_field(msg, 0, Val_int(x));
    Store_field(msg, 1, Val_int(y));
    Store_field(msg, 2, Val_long(state));
    Store_field(msg, 3, Val_int(direction));
    callback(win_id, msg);
}

void notify_scroll(unsigned win_id, int x, int y, unsigned state, int dx, int dy)
{
    value msg = caml_alloc(5, Tag_Scroll);
    Store_field(msg, 0, Val_int(x));
    Store_field(msg, 1, Val_int(y));
    Store_field(msg, 2, Val_long(state));
    Store_field(msg, 3, Val_int(dx));
    Store_field(msg, 4, Val_int(dy));
    callback(win_id, msg);
}

value make_scale_msg(double scale)
{
    CAMLparam0();
    CAMLlocal2(v_scale, msg);
    msg = caml_alloc(1, Tag_Scale);
    v_scale = caml_copy_double(scale);
    Store_field(msg, 0, v_scale);
    CAMLreturn(msg);
}

void notify_scale(unsigned win_id, double scale)
{
    value msg = make_scale_msg(scale);
    callback(win_id, msg);
}

void notify_key_down(unsigned win_id, int keycode, unsigned state)
{
    value msg = caml_alloc(2, Tag_KeyDown);
    Store_field(msg, 0, Val_int(keycode));
    Store_field(msg, 1, Val_long(state));
    callback(win_id, msg);
}

void notify_key_up(unsigned win_id, int keycode, unsigned state)
{
    value msg = caml_alloc(2, Tag_KeyUp);
    Store_field(msg, 0, Val_int(keycode));
    Store_field(msg, 1, Val_long(state));
    callback(win_id, msg);
}

void notify_char(unsigned win_id, int c)
{
    value msg = caml_alloc(1, Tag_Char);
    Store_field(msg, 0, Val_int(c));
    callback(win_id, msg);
}
