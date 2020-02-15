#define CAML_NAME_SPACE
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <gtk/gtk.h>
#include <cairo-svg.h>
#include <cairo-pdf.h>
#include "win.h"

enum res_type {
    RES_TYPE_WINDOW,
    RES_TYPE_FONT,
    NUM_RES_TYPES,
};

unsigned res_length_table[] = {
    256,                         /* window */
    256,                         /* font */
};

typedef struct res res_t;

struct res {
    void* object;
    res_t *link;
};

typedef struct res_book res_book_t;

struct res_book {
    res_t *v;
    res_t *free_list;
    int length;
};

typedef struct res_window {
    bool active;
    int id;
    GtkWidget *window;
    GtkWidget *drawing_area;
    int width;
    int height;
} res_window;

typedef struct res_font {
    int id;
    cairo_font_face_t *face;
    char *family;
    int slant_index;
    int weight_index;
    double size;
    cairo_font_extents_t extents;
    double space_width;         /* because text_extents ignores spaces */
} res_font;

res_book_t res_book_table[NUM_RES_TYPES];
GtkApplication *app;
cairo_surface_t *surface_image;
cairo_surface_t *surface_save;
cairo_t *cr;
cairo_t *cr_image;
res_font *cur_font = NULL;
cairo_font_extents_t font_extents;

#define FONT_STACK_LENGTH 256
res_font *font_stack[FONT_STACK_LENGTH];
int font_stack_index;

#define TRANSFORM_STACK_LENGTH  256
cairo_matrix_t transform_stack[TRANSFORM_STACK_LENGTH];
int transform_stack_index;

typedef struct rect {
    int x;
    int y;
    int w;
    int h;
} rect_t;

#define CLIP_STACK_LENGTH 256
rect_t clip_stack[CLIP_STACK_LENGTH];
int clip_stack_index;

static inline int min(int x, int y) { return x <= y ? x : y; }
static inline int max(int x, int y) { return x >= y ? x : y; }

void rect_intersection(rect_t *r, const rect_t *p, const rect_t *q)
{
    int x0 = max(p->x, q->x);
    int y0 = max(p->y, q->y);
    int x1 = min(p->x + p->w, q->x + q->w);
    int y1 = min(p->y + p->h, q->y + q->h);
    if (x0 < x1 && y0 < y1) {
        r->x = x0;
        r->y = y0;
        r->w = x1 - x0;
        r->h = y1 - y0;            
    } else {
        r->x = r->y = r->w = r->h = 0;
    }
}

void res_book_init(void)
{
    for (int type = 0; type < NUM_RES_TYPES; ++type) {
        res_book_t *bp = &res_book_table[type];
        bp->length = res_length_table[type];
        bp->v = malloc(bp->length * sizeof(res_t));
        bp->free_list = NULL;
        for (int i = bp->length; i > 0; ) {
            i--;
            bp->v[i].link = bp->free_list;
            bp->free_list = &bp->v[i];
        }
    }
}

void *res_get(int type, int i)
{
    return res_book_table[type].v[i].object;
}

res_t *res_alloc(int type, void *object, int *p_index)
{
    res_book_t *bp = &res_book_table[type];
    assert(bp->free_list != NULL);
    res_t *p = bp->free_list;
    bp->free_list = p->link;
    p->link = NULL;
    p->object = object;
    *p_index = p - bp->v;
    return p;
}

void res_free(unsigned type, int i)
{
    res_book_t *bp = &res_book_table[type];
    res_t *p = &bp->v[i];
    p->link = bp->free_list;
    bp->free_list = p;
}

unsigned translate_event_state(guint event_state)
{
    unsigned state = 0;
    if (event_state & GDK_SHIFT_MASK) {
        state |= keyStateShift;
    }
#ifndef __APPLE__
    if (event_state & GDK_CONTROL_MASK) {
        state |= keyStateControl;
    }
    if (event_state & GDK_MOD2_MASK) { /* ? */
        state |= keyStateMeta;
    }
#else
    if (event_state & GDK_MOD2_MASK) { /* Command key */
        state |= keyStateControl;
    }
    if (event_state & GDK_CONTROL_MASK) {
        state |= keyStateMeta;
    }
#endif
    if (event_state & GDK_MOD1_MASK) {
        state |= keyStateAlt;
    }
    if (event_state & GDK_BUTTON1_MASK) {
        state |= mouseButtonLeft;
    }
    if (event_state & GDK_BUTTON2_MASK) {
        state |= mouseButtonMiddle;
    }
    if (event_state & GDK_BUTTON3_MASK) {
        state |= mouseButtonRight;
    }
    return state;
}

unsigned translate_event_button(guint button)
{
    if (button == GDK_BUTTON_PRIMARY) {
        return mouseButtonLeft;
    } else if (button == GDK_BUTTON_SECONDARY) {
        return mouseButtonRight;
    } else if (button == GDK_BUTTON_MIDDLE) {
        return mouseButtonMiddle;
    } else {
        return 0;
    }
}

gboolean key_press(GtkWidget *widget, GdkEventKey *event, res_window *win)
{
    unsigned state = translate_event_state(event->state);
    if (event->type == GDK_KEY_PRESS) {
        notify_key_down(win->id, event->keyval, state);
    } else {
        notify_key_up(win->id, event->keyval, state);
    }
    return TRUE;
}

gboolean cb_idle_winsize(gpointer user_data)
{
    res_window *win = (res_window *)user_data;
    if (win->active) {
        notify_win_size(win->id, win->width, win->height);
        GdkRectangle r;
        r.x = 0;
        r.y = 0;
        r.width = win->width;
        r.height = win->height;
        gdk_window_invalidate_rect(gtk_widget_get_window(win->drawing_area), &r, FALSE);
        return FALSE;
    } else {
        g_print("cb_idle_winsize: the win is not active\n");
        return TRUE;
    }
}

gboolean configure_event_cb(GtkWidget *widget, GdkEventConfigure *event, res_window *win)
{
    win->width = gtk_widget_get_allocated_width(widget);
    win->height = gtk_widget_get_allocated_height(widget);
    if (win->active) {
        notify_win_size(win->id, win->width, win->height);
    } else {
        g_idle_add(&cb_idle_winsize, win);
    }
    return TRUE;
}

gboolean draw_cb(GtkWidget *widget, cairo_t *cr0, res_window *win)
{
    cr = cr0;
    cur_font = NULL;
    notify_paint(win->id, 0, 0, win->width, win->height);
    cur_font = NULL;
    cr = cr_image;
    return FALSE;
}

gboolean button_press_event_cb(GtkWidget *widget, GdkEventButton *event, res_window *win)
{
    unsigned button = translate_event_button(event->button);
    unsigned state = translate_event_state(event->state);
    if (event->type == GDK_2BUTTON_PRESS) {
        state |= mouseButtonStateDoubleClick;
    }
    notify_mouse_down(win->id, event->x, event->y, state, button);
    return TRUE;
}

gboolean button_release_event_cb(GtkWidget *widget, GdkEventButton *event, res_window *win)
{
    unsigned button = translate_event_button(event->button);
    unsigned state = translate_event_state(event->state);
    notify_mouse_up(win->id, event->x, event->y, state, button);
    return TRUE;
}

gboolean motion_notify_event_cb(GtkWidget *widget, GdkEventMotion *event, res_window *win)
{
    unsigned state = translate_event_state(event->state);
    notify_mouse_move(win->id, event->x, event->y, state);
    return TRUE;
}

static gboolean scroll_event_cb(GtkWidget *widget, GdkEventScroll *event, res_window *win)
{
    unsigned state = translate_event_state(event->state);
    int direction = event->direction == GDK_SCROLL_UP ? 1 : -1;

    gdouble dx, dy;
    gboolean b = gdk_event_get_scroll_deltas((GdkEvent *)event, &dx, &dy);

    notify_mouse_wheel(win->id, event->x, event->y, state, direction);

    if (event->direction == GDK_SCROLL_UP) {
        dx = event->delta_x; dy = event->delta_y;
    } else if (event->direction == GDK_SCROLL_DOWN) {
        dx = event->delta_x; dy = -event->delta_y;
    } else if (event->direction == GDK_SCROLL_LEFT) {
        dx = event->delta_x; dy = event->delta_y;
    } else {
        dx = -event->delta_x; dy = event->delta_y;
    }
    notify_scroll(win->id, event->x, event->y, state, dx * 5.0, dy * 5.0);
    return TRUE;
}

static void zoom_begin_cb(GtkGesture *gesture, GdkEventSequence *sequence, res_window *win)
{
}

static void zoom_scale_changed_cb(GtkGestureZoom *zoom, gdouble scale, res_window *win)
{
    notify_scale(win->id, scale);
}

gboolean enter_notify_event_cb(GtkWidget *widget, GdkEventCrossing *event, res_window *win)
{
    notify_mouse_enter(win->id);
    return TRUE;
}

gboolean leave_notify_event_cb(GtkWidget *widget, GdkEventCrossing *event, res_window *win)
{
    notify_mouse_leave(win->id);
    return TRUE;
}

void close_window(GtkWidget *widget, res_window *win)
{
    notify_end_session();
    res_free(RES_TYPE_WINDOW, win->id);
    free(win);
}

gboolean close_button(GtkWidget *widget, GdkEvent *event, res_window *win)
{
    notify_win_close(win->id);
    return FALSE;
}

void activate(GtkApplication *app, char **argv)
{
    surface_image = cairo_image_surface_create (CAIRO_FORMAT_ARGB32, 1, 1);
    cr_image = cairo_create(surface_image);
    cr = cr_image;
    guedra_init();
}

CAMLprim value guedra_create_window(value vtitle, value vx, value vy, value vw, value vh)
{
    char *title = String_val(vtitle);
    long x = Long_val(vx);
    long y = Long_val(vy);
    long w = Long_val(vw);
    long h = Long_val(vh);

    res_window *win = malloc(sizeof(res_window));
    win->active = false;

    res_t *p = res_alloc(RES_TYPE_WINDOW, win, &win->id);

    win->window = gtk_application_window_new(app);
    gtk_window_set_default_size(GTK_WINDOW(win->window), w, h);
    gtk_window_set_title(GTK_WINDOW(win->window), title);

    g_signal_connect(win->window, "destroy", G_CALLBACK(close_window), win);
    g_signal_connect(win->window, "delete-event", G_CALLBACK(close_button), win);
    g_signal_connect(win->window, "key-press-event", G_CALLBACK(key_press), win);
    g_signal_connect(win->window, "key-release-event", G_CALLBACK(key_press), win);

    win->drawing_area = gtk_drawing_area_new();
    gtk_widget_set_size_request(win->drawing_area, 100, 100);

    gtk_container_add(GTK_CONTAINER(win->window), win->drawing_area);

    g_signal_connect(win->drawing_area, "draw", G_CALLBACK(draw_cb), win);
    g_signal_connect(win->drawing_area, "configure-event", G_CALLBACK(configure_event_cb), win);

    g_signal_connect(win->drawing_area, "motion-notify-event",
                     G_CALLBACK(motion_notify_event_cb), win);
    g_signal_connect(win->drawing_area, "button-press-event",
                     G_CALLBACK(button_press_event_cb), win);
    g_signal_connect(win->drawing_area, "button-release-event",
                     G_CALLBACK(button_release_event_cb), win);
    g_signal_connect(win->drawing_area, "enter-notify-event",
                     G_CALLBACK(enter_notify_event_cb), win);
    g_signal_connect(win->drawing_area, "leave-notify-event",
                     G_CALLBACK(leave_notify_event_cb), win);
    g_signal_connect(win->drawing_area, "scroll-event",
                     G_CALLBACK(scroll_event_cb), win);

    gtk_widget_set_events(win->drawing_area,
                          gtk_widget_get_events(win->drawing_area)
                          | GDK_STRUCTURE_MASK 
                          | GDK_BUTTON_PRESS_MASK
                          | GDK_BUTTON_RELEASE_MASK
                          | GDK_POINTER_MOTION_MASK
                          | GDK_SCROLL_MASK
                          | GDK_ENTER_NOTIFY_MASK
                          | GDK_LEAVE_NOTIFY_MASK);

    GtkGesture *zoom;
    zoom = gtk_gesture_zoom_new (GTK_WIDGET(win->drawing_area));
    g_signal_connect(zoom, "begin",
                     G_CALLBACK(zoom_begin_cb), win);
    g_signal_connect(zoom, "scale-changed",
                     G_CALLBACK(zoom_scale_changed_cb), win);

    gtk_widget_show_all(win->window);
    win->active = true;
    return Val_int(win->id);
}

CAMLprim value guedra_prn(value vsMsg)
{
    g_print("%s\n", String_val(vsMsg));
    return Val_unit;
}

CAMLprim value guedra_invalidate(value win_id, value x, value y, value w, value h)
{
    res_window *win = res_get(RES_TYPE_WINDOW, Long_val(win_id));
    assert(win != NULL);
    if (win->window != NULL) {
        GdkRectangle r;
        r.x = Long_val(x);
        r.y = Long_val(y);
        r.width = Long_val(w);
        r.height = Long_val(h);
        gdk_window_invalidate_rect(gtk_widget_get_window(win->drawing_area), &r, FALSE);
    }
    return Val_unit;
}

CAMLprim value guedra_set_color(value color) /* tuple (r, g, b) */
{
    assert(Tag_val(color) == Double_array_tag);
    assert(Wosize_val(color) == 3);
    double r = Double_field(color, 0);
    double g = Double_field(color, 1);
    double b = Double_field(color, 2);
    cairo_set_source_rgb(cr, r, g, b);
    return Val_unit;
}

CAMLprim value guedra_set_line_width(value w)
{
    cairo_set_line_width(cr, Double_val(w));
    return Val_unit;
}

CAMLprim value guedra_draw_line(value x0, value y0, value x1, value y1)
{
    cairo_move_to(cr, Double_val(x0), Double_val(y0));
    cairo_line_to(cr, Double_val(x1), Double_val(y1));
    cairo_stroke(cr);
    return Val_unit;
}

CAMLprim value guedra_draw_rect(value x, value y, value w, value h)
{
    cairo_rectangle(cr, Double_val(x), Double_val(y), Double_val(w), Double_val(h));
    cairo_stroke(cr);
    return Val_unit;
}

CAMLprim value guedra_fill_rect(value x, value y, value w, value h)
{
    cairo_rectangle(cr, Double_val(x), Double_val(y), Double_val(w), Double_val(h));
    cairo_fill(cr);
    return Val_unit;
}

CAMLprim value make_font(value vsFamily, value vlSlant, value vlWeight, value vdSize)
{
    res_font *font = (res_font *)malloc(sizeof(res_font));
    res_t *p = res_alloc(RES_TYPE_FONT, font, &font->id);

    char *s = String_val(vsFamily);
    size_t len = strlen(s);
    font->family = malloc(len + 1);
    strcpy(font->family, s);
    font->slant_index = Int_val(vlSlant);
    font->weight_index = Int_val(vlWeight);
    font->size = Double_val(vdSize);
    font->extents.height = 0.0;

    cairo_font_slant_t slant;
    cairo_font_weight_t weight;

    if (font->slant_index == 0)
        slant = CAIRO_FONT_SLANT_NORMAL;
    else if (font->slant_index == 1)
        slant = CAIRO_FONT_SLANT_ITALIC;
    else
        slant = CAIRO_FONT_SLANT_OBLIQUE;

    if (font->weight_index == 0)
        weight = CAIRO_FONT_WEIGHT_NORMAL;
    else
        weight = CAIRO_FONT_WEIGHT_BOLD;

    font->face = cairo_toy_font_face_create(font->family, slant, weight);
    return Val_int(font->id);
}

CAMLprim value delete_font(value vlFont_id)
{
    int font_id = Int_val(vlFont_id);
    res_font *font = (res_font *)res_get(RES_TYPE_FONT, font_id);
    res_free(RES_TYPE_FONT, font_id);
    cairo_font_face_destroy(font->face);
    free(font->family);
    free(font);
    return Val_unit;
}

CAMLprim value set_font(value vlFont_id)
{
    int font_id = Int_val(vlFont_id);
    res_font *font = (res_font *)res_get(RES_TYPE_FONT, font_id);
    res_font *font_prev = cur_font;

    if (cur_font != font) {
        cur_font = font;
        cairo_set_font_face(cr, font->face);
        cairo_set_font_size(cr, font->size);
    }

    if (font->extents.height == 0.0) {
        cairo_font_extents(cr, &font->extents);
        cairo_text_extents_t extents;
        cairo_text_extents(cr, "| |", &extents);
        double w = extents.width;
        cairo_text_extents(cr, "||", &extents);
        font->space_width = w - extents.width;
    }
    return Val_unit;
}

CAMLprim value guedra_draw_text(value x, value y, value vsText)
{
    char *text = String_val(vsText);
    cairo_move_to(cr, Double_val(x), Double_val(y) + cur_font->extents.ascent);
    cairo_show_text(cr, text);
    return Val_unit;
}

CAMLprim value push_clip(value vx, value vy, value vw, value vh)
{
    double x = Int_val(vx);
    double y = Int_val(vy);
    double w = Int_val(vw);
    double h = Int_val(vh);

    cairo_save(cr);
    cairo_rectangle(cr, x, y, w, h);
    cairo_clip(cr);

    return Val_unit;
}

CAMLprim value pop_clip(value u)
{
    cairo_restore(cr);
    return Val_unit;
}

static void push_transform(const cairo_matrix_t *p)
{
    cairo_save(cr);
    cairo_transform(cr, p);
    font_stack[font_stack_index++] = cur_font;
}

static void pop_tranform(void)
{
    cairo_restore(cr);
    cur_font = font_stack[--font_stack_index];
}

CAMLprim value guedra_pop_transform(value u)
{
    pop_tranform();
    return Val_unit;
}

CAMLprim value guedra_push_translate(value vx, value vy)
{
    assert(transform_stack_index < TRANSFORM_STACK_LENGTH);
    cairo_matrix_t m;
    cairo_matrix_init_translate(&m, Double_val(vx), Double_val(vy));
    push_transform(&m);
    return Val_unit;
}

CAMLprim value guedra_push_translate_scale(value vx, value vy, value vscale)
{
    assert(transform_stack_index < TRANSFORM_STACK_LENGTH);
    cairo_matrix_t m;
    m.x0 = Double_val(vx);
    m.y0 = Double_val(vy);
    m.xx = m.yy = Double_val(vscale);
    m.xy = m.yx = 0.0;
    push_transform(&m);
    return Val_unit;
}

CAMLprim value guedra_fill_bezier(value v)
{
    long n = Wosize_val(v);
    if (n == 0)
        return Val_unit;
    value p = Field(v, 0);
    double x = Double_val(Field(p, 0));
    double y = Double_val(Field(p, 1));
    cairo_move_to(cr, x, y);

    for (long i = 1; i + 3 <= n; i += 3) {
        value p0 = Field(v, i);
        double x0 = Double_val(Field(p0, 0));
        double y0 = Double_val(Field(p0, 1));
        value p1 = Field(v, i + 1);
        double x1 = Double_val(Field(p1, 0));
        double y1 = Double_val(Field(p1, 1));
        value p2 = Field(v, i + 2);
        double x2 = Double_val(Field(p2, 0));
        double y2 = Double_val(Field(p2, 1));
        cairo_curve_to(cr, x0, y0, x1, y1, x2, y2);
    }
    cairo_fill(cr);
    return Val_unit;
}

CAMLprim value guedra_text_extents(value vs_s)
{
    CAMLparam1(vs_s);
    CAMLlocal1(v);
    char *s = String_val(vs_s);
    int nsp = 0;
    while (*s == ' ') { s++; nsp++; }
    cairo_text_extents_t extents;
    cairo_text_extents(cr, s, &extents);
    v = caml_alloc(3, Double_array_tag);
    Store_double_field(v, 0, extents.width + nsp * cur_font->space_width);
    Store_double_field(v, 1, cur_font->extents.height);
    Store_double_field(v, 2, cur_font->extents.ascent);
    CAMLreturn(v);
}

CAMLprim value guedra_set_dash(value va_dashes, value vf_offset)
{
    double offset = Double_val(vf_offset);
    int n = Wosize_val(va_dashes);
    double *dashes = alloca(n * sizeof(double));
    int tag = Tag_val(va_dashes);
    if (tag == Double_array_tag) {
        for (int i = 0; i < n; ++i) {
            dashes[i] = Double_field(va_dashes, i);
        }
    } else {
        for (int i = 0; i < n; ++i) {
            dashes[i] = Double_val(Field(va_dashes, i));
        }
    }
    cairo_set_dash(cr, dashes, n, offset);
    return Val_unit;
}

CAMLprim value guedra_set_dash_symmetric(value vf_d, value vf_offset)
{
    double offset = Double_val(vf_offset);
    double dashes[1];
    dashes[0] = Double_val(vf_d);
    cairo_set_dash(cr, dashes, 1, offset);
    return Val_unit;
}

CAMLprim value guedra_set_dash_solid(value v)
{
    cairo_set_dash(cr, NULL, 0, 0.0);
    return Val_unit;
}

CAMLprim value imp_create_svg_surface(value vs_filename, value vf_width, value vf_height)
{
    assert(surface_save == NULL);
    const char *filename = String_val(vs_filename);
    double width = Double_val(vf_width);
    double height = Double_val(vf_height);
    surface_save = cairo_svg_surface_create(filename, width, height);
    cr = cairo_create(surface_save);
    return Val_unit;
}

CAMLprim value imp_create_pdf_surface(value vs_filename, value vf_width, value vf_height)
{
    assert(surface_save == NULL);
    const char *filename = String_val(vs_filename);
    double width = Double_val(vf_width);
    double height = Double_val(vf_height);
    surface_save = cairo_pdf_surface_create(filename, width, height);
    cr = cairo_create(surface_save);
    return Val_unit;
}

CAMLprim value imp_delete_surface(value v)
{
    assert(surface_save != NULL);
    cairo_destroy(cr);
    cr = cr_image;
    cairo_surface_destroy(surface_save);
    surface_save = NULL;
    return Val_unit;
}

CAMLprim value imp_clipboard_set_text(value vs)
{
    char *s = String_val(vs);
    size_t n = caml_string_length(vs);
    GtkClipboard* clipboard = gtk_clipboard_get(GDK_SELECTION_CLIPBOARD);
    gtk_clipboard_set_text(clipboard, s, n);

    return Val_unit;
}

CAMLprim value imp_guedra_quit(value vs)
{
    g_application_quit((GApplication *)app);
    return Val_unit;
}

int main(int argc, char **argv)
{
    int argc_dummy = 1;
    char *argv_dummy[1];
    argv_dummy[0] = argv[0];
    res_book_init();
    caml_main(argv);
    app = gtk_application_new("org.gtk.example", G_APPLICATION_FLAGS_NONE);
    g_signal_connect(app, "activate", G_CALLBACK(activate), argv_dummy);
    int status = g_application_run(G_APPLICATION(app), argc_dummy, argv_dummy);
    g_object_unref(app);
    return status;
}
