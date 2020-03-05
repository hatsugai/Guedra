#include <sdkddkver.h>
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <winsock2.h>
#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include <memory.h>
#include <d2d1.h>
#include <dwrite.h>
#include <assert.h>

#undef _T
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>
#undef _T

#include "win.h"

#define WM_RESEND_SIZE  WM_APP

#define TRANSFORM_STACK_LENGTH  32

HINSTANCE hInstance;

ID2D1Factory* pD2DFactory;
IDWriteFactory* pDWriteFactory;
RECT clientRect;
//ID2D1HwndRenderTarget* pRT;
ID2D1RenderTarget* pRT;
ID2D1SolidColorBrush* pBrush;
double line_width;
IDWriteTextFormat *pTextFormat;
WCHAR text_buf[65536];
D2D_MATRIX_3X2_F transform_stack[TRANSFORM_STACK_LENGTH];
int transform_stack_index;
ID2D1StrokeStyle *strokeStyle;
ID2D1BitmapRenderTarget *bitmapRenderTarget;

unsigned g_MouseButtonStates;
unsigned g_KeyStates;

enum res_type {
    RES_TYPE_WINDOW,
    RES_TYPE_FONT,
    NUM_RES_TYPES,
};

unsigned res_length_table[] = {
    256,                        /* window */
    256,                        /* font */
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
    HWND hwnd;
    int width;
    int height;
} res_window;

typedef struct res_font {
    int id;
    IDWriteTextFormat *pTextFormat;
    wchar_t *family;
    int slant_index;
    int weight_index;
    double font_size;
} res_font;

res_book_t res_book_table[NUM_RES_TYPES];

extern "C" void res_book_init(void)
{
    for (int type = 0; type < NUM_RES_TYPES; ++type) {
        res_book_t *bp = &res_book_table[type];
        bp->length = res_length_table[type];
        bp->v = (res_t *)malloc(bp->length * sizeof(res_t));
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

template<class Interface>
inline void SafeRelease(
    Interface **ppInterfaceToRelease
    )
{
    if (*ppInterfaceToRelease != NULL)
    {
        (*ppInterfaceToRelease)->Release();

        (*ppInterfaceToRelease) = NULL;
    }
}

wchar_t *clone_string(const wchar_t *s)
{
    size_t n = wcslen(s);
    wchar_t *t = (wchar_t *)malloc((n + 1) * sizeof(wchar_t));
    memcpy(t, s, (n + 1) * sizeof(wchar_t));
    return t;
}

wchar_t *convert_string(const char *s)
{
#ifdef _UNICODE
    size_t length = strlen(s);
    for (size_t i = 0; i < length; ++i) {
        text_buf[i] = s[i];
    }
    text_buf[length] = 0;
    return text_buf;
#else
    return s;
#endif
}

int convert_keycode(int k)
{
    int key;
    switch (k) {
    case VK_BACK:       key = keyBackSpace; break;
    case VK_TAB:        key = keyTab;       break;
    case VK_RETURN:     key = keyReturn;    break;
    case VK_ESCAPE:     key = keyEscape;    break;
    case VK_DELETE:     key = keyDelete;    break;
    case VK_HOME:       key = keyHome;      break;
    case VK_LEFT:       key = keyLeft;      break;
    case VK_UP:         key = keyUp;        break;
    case VK_RIGHT:      key = keyRight;     break;
    case VK_DOWN:       key = keyDown;      break;
    case VK_PRIOR:      key = keyPageUp;    break;
    case VK_NEXT:       key = keyPageDown;  break;
    case VK_END:        key = keyEnd;       break;
    case VK_INSERT:     key = keyInsert;    break;
    case VK_F1:         key = keyF1;        break;
    case VK_F2:         key = keyF2;        break;
    case VK_F3:         key = keyF3;        break;
    case VK_F4:         key = keyF4;        break;
    case VK_F5:         key = keyF5;        break;
    case VK_F6:         key = keyF6;        break;
    case VK_F7:         key = keyF7;        break;
    case VK_F8:         key = keyF8;        break;
    case VK_F9:         key = keyF9;        break;
    case VK_F10:        key = keyF10;       break;
    case VK_F11:        key = keyF11;       break;
    case VK_F12:        key = keyF12;       break;
    case VK_SHIFT:      key = keyShift;     break;
    case VK_LSHIFT:     key = keyShift;     break;
    case VK_RSHIFT:     key = keyShift;     break;
    case VK_CONTROL:    key = keyControl;   break;
    case VK_LCONTROL:   key = keyControl;   break;
    case VK_RCONTROL:   key = keyControl;   break;
    case VK_MENU:       key = keyAlt;       break;
    case VK_LMENU:      key = keyAlt;       break;
    case VK_RMENU:      key = keyAlt;       break;
    default:
        if (k >= 'A' && k <= 'Z' && !(g_KeyStates & keyStateShift)) {
            key = k - 'A' + 'a';
        } else {
            key = k;
        }
    }
    return key;
}

void SendMouseButtonMsg(HWND hwnd, int btn, bool b_down, bool b_dblclk,
                        WPARAM wParam, LPARAM lParam) {
    int win_id = GetWindowLong(hwnd, 0);
    if (b_down) {
        if (!g_MouseButtonStates) {
            SetCapture(hwnd);
        }
        g_MouseButtonStates |= btn;
    } else {
        g_MouseButtonStates &= ~btn;
        if (!g_MouseButtonStates && GetCapture() == hwnd) {
            ReleaseCapture();
        }
    }

    unsigned states = g_MouseButtonStates | g_KeyStates;

    if (b_dblclk) {
        states |= mouseButtonStateDoubleClick;
    }

    if (b_down) {
        notify_mouse_down(win_id, LOWORD(lParam), HIWORD(lParam), states, btn);
    } else {
        notify_mouse_up(win_id, LOWORD(lParam), HIWORD(lParam), states, btn);
    }
}

LRESULT CALLBACK WndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam) {
    switch (message) {
    case WM_CREATE:
        {
            /* initialize win->hwnd and WindowLong (before WM_SIZE is sent) */
            CREATESTRUCT *p = (CREATESTRUCT *)lParam;
            res_window *win = (res_window *)(p->lpCreateParams);
            win->hwnd = hWnd;
            SetWindowLong(win->hwnd, 0, win->id);
        }
        return 0;
    case WM_SIZE:
        {
            int win_id = GetWindowLong(hWnd, 0);
            res_window *win = (res_window *)res_get(RES_TYPE_WINDOW, win_id);
            win->width = clientRect.right = LOWORD(lParam);
            win->height = clientRect.bottom = HIWORD(lParam);
            if (win->active) {
                notify_win_size(win_id, win->width, win->height);
            } else {
                PostMessage(hWnd, WM_RESEND_SIZE, 0, 0);
            }
        }
        break;
    case WM_RESEND_SIZE:
        {
            int win_id = GetWindowLong(hWnd, 0);
            res_window *win = (res_window *)res_get(RES_TYPE_WINDOW, win_id);
            win->active = true;
            notify_win_size(win_id, win->width, win->height);
            InvalidateRect(hWnd, NULL, FALSE);
        }
        break;

    case WM_LBUTTONDOWN:
        SendMouseButtonMsg(hWnd, mouseButtonLeft,   true,   false,  wParam, lParam);
        break;
    case WM_LBUTTONDBLCLK:
        SendMouseButtonMsg(hWnd, mouseButtonLeft,   true,   true,   wParam, lParam);
        break;
    case WM_LBUTTONUP:
        SendMouseButtonMsg(hWnd, mouseButtonLeft,   false,  false,  wParam, lParam);
        break;
    case WM_RBUTTONDOWN:
        SendMouseButtonMsg(hWnd, mouseButtonRight,  true,   false,  wParam, lParam);
        break;
    case WM_RBUTTONDBLCLK:
        SendMouseButtonMsg(hWnd, mouseButtonRight,  true,   true,   wParam, lParam);
        break;
    case WM_RBUTTONUP:
        SendMouseButtonMsg(hWnd, mouseButtonRight,  false,  false,  wParam, lParam);
        break;
    case WM_MBUTTONDOWN:
        SendMouseButtonMsg(hWnd, mouseButtonMiddle, true,   false,  wParam, lParam);
        break;
    case WM_MBUTTONDBLCLK:
        SendMouseButtonMsg(hWnd, mouseButtonMiddle, true,   true,   wParam, lParam);
        break;
    case WM_MBUTTONUP:
        SendMouseButtonMsg(hWnd, mouseButtonMiddle, false,  false,  wParam, lParam);
        break;

    case WM_MOUSEMOVE:
        {
            int win_id = GetWindowLong(hWnd, 0);
            unsigned states = g_MouseButtonStates | g_KeyStates;
            notify_mouse_move(win_id, LOWORD(lParam), HIWORD(lParam), states);
        }
        break;

    case WM_MOUSEWHEEL:
        {
            int win_id = GetWindowLong(hWnd, 0);
            unsigned states = g_MouseButtonStates | g_KeyStates;
            POINT p;
            p.x = LOWORD(lParam);
            p.y = HIWORD(lParam);
            ScreenToClient(hWnd, &p);
            int16_t d = HIWORD(wParam);
            notify_mouse_wheel(win_id, p.x, p.y, states, d / WHEEL_DELTA);
        }
        break;

    case WM_SYSKEYDOWN:
    case WM_KEYDOWN:
        {
            int win_id = GetWindowLong(hWnd, 0);
            int keycode = convert_keycode(wParam);
            if (lParam & 0x40000000) { // previous key state
                g_KeyStates |= keyStateRepeat;
            }
            switch (keycode) {
            case keyShift:   g_KeyStates |= keyStateShift;   break;
            case keyControl: g_KeyStates |= keyStateControl; break;
            case keyAlt:     g_KeyStates |= keyStateAlt;     break;
            }
            notify_key_down(win_id, keycode, g_MouseButtonStates | g_KeyStates);
        }
        break;

    case WM_SYSKEYUP:
    case WM_KEYUP:
        {
            int win_id = GetWindowLong(hWnd, 0);
            int keycode = convert_keycode(wParam);
            g_KeyStates &= ~keyStateRepeat;
            switch (keycode) {
            case keyShift:   g_KeyStates &= ~keyStateShift;   break;
            case keyControl: g_KeyStates &= ~keyStateControl; break;
            case keyAlt:     g_KeyStates &= ~keyStateAlt;     break;
            }
            notify_key_up(win_id, keycode, g_MouseButtonStates | g_KeyStates);
        }
        break;

    case WM_CHAR:
        {
            int win_id = GetWindowLong(hWnd, 0);
            notify_char(win_id, wParam);
        }
        break;

    case WM_PAINT:
        {
            PAINTSTRUCT ps;
            HDC hdc = BeginPaint(hWnd, &ps);
            int win_id = GetWindowLong(hWnd, 0);
            res_window *win = (res_window *)res_get(RES_TYPE_WINDOW, win_id);
            if (win->active) {
                RECT rc;
                GetClientRect(hWnd, &rc);
				ID2D1HwndRenderTarget *pRThwnd;
                HRESULT hr = pD2DFactory->CreateHwndRenderTarget
                    (D2D1::RenderTargetProperties(),
                     D2D1::HwndRenderTargetProperties
                     (hWnd,
                      D2D1::SizeU(rc.right - rc.left, rc.bottom - rc.top)),
                     &pRThwnd);
				pRT = pRThwnd;
				if (bitmapRenderTarget == NULL) {
					pRT->CreateCompatibleRenderTarget(&bitmapRenderTarget);
				}

                pRT->BeginDraw();
                pRT->SetTransform(D2D1::IdentityMatrix());
                pRT->Clear(D2D1::ColorF(D2D1::ColorF::White));
                notify_paint(win_id,
                             ps.rcPaint.left, ps.rcPaint.top,
                             ps.rcPaint.right - ps.rcPaint.left,
                             ps.rcPaint.bottom - ps.rcPaint.top);
                hr = pRT->EndDraw();
                SafeRelease(&pRT);
                if (pBrush != NULL) {
                    SafeRelease(&pBrush);
                }
            }
            EndPaint(hWnd, &ps);
        }
        break;
    case WM_DESTROY:
        PostQuitMessage(0);
        break;
    default:
        return DefWindowProc(hWnd, message, wParam, lParam);
    }
    return 0;
}

extern "C" {

CAMLprim value guedra_create_window(value vtitle, value vx, value vy, value vw, value vh)
{
    wchar_t *title = convert_string(String_val(vtitle));
    long x = Long_val(vx);
    long y = Long_val(vy);
    long w = Long_val(vw);
    long h = Long_val(vh);

    res_window *win = (res_window *)malloc(sizeof(res_window));
    win->active = false;
    res_t *p = res_alloc(RES_TYPE_WINDOW, win, &win->id);

    HWND hwnd = CreateWindow(L"GUEDRA", title, WS_OVERLAPPEDWINDOW,
                             x, y, w, h, nullptr, nullptr, hInstance, win);
    assert(hwnd == win->hwnd);
    assert(GetWindowLong(hwnd, 0) == win->id);
    ShowWindow(win->hwnd, SW_SHOW);
    return Val_int(win->id);
}

CAMLprim value guedra_prn(value v)
{
    printf("%s", String_val(v));
    fflush(stdout);
    return Val_unit;
}

CAMLprim value guedra_invalidate(value win_id, value x, value y, value w, value h)
{
    res_window *win = (res_window *)res_get(RES_TYPE_WINDOW, Long_val(win_id));
    RECT r;
    r.left = Long_val(x);
    r.right = r.left + Long_val(w);
    r.top = Long_val(y);
    r.bottom = r.top + Long_val(h);
    InvalidateRect(win->hwnd, &r, FALSE);
    return Val_unit;
}

CAMLprim value guedra_set_color(value color) { /* record { red; green; blue; } */
    assert(Tag_val(color) == Double_array_tag);
    assert(Wosize_val(color) == 3);
    double r = Double_field(color, 0);
    double g = Double_field(color, 1);
    double b = Double_field(color, 2);
    if (pBrush != NULL) {
        SafeRelease(&pBrush);
    }
    pRT->CreateSolidColorBrush(D2D1::ColorF(r, g, b), &pBrush);
    return Val_unit;
}

CAMLprim value guedra_set_line_width(value w) {
    line_width = Double_val(w);
    return Val_unit;
}

CAMLprim value guedra_draw_line(value x0, value y0, value x1, value y1) {
    pRT->DrawLine(D2D1::Point2F(Double_val(x0), Double_val(y0)),
                  D2D1::Point2F(Double_val(x1), Double_val(y1)),
                  pBrush, line_width, strokeStyle);
    return Val_unit;
}

CAMLprim value guedra_draw_rect(value val_x, value val_y, value val_w, value val_h) {
    double x = Double_val(val_x);
    double y = Double_val(val_y);
    double w = Double_val(val_w);
    double h = Double_val(val_h);
    pRT->DrawRectangle(D2D1::RectF(x, y, x + w, y + h),
					   pBrush, line_width, strokeStyle);
    return Val_unit;
}

CAMLprim value guedra_fill_rect(value val_x, value val_y, value val_w, value val_h) {
    double x = Double_val(val_x);
    double y = Double_val(val_y);
    double w = Double_val(val_w);
    double h = Double_val(val_h);
    pRT->FillRectangle(D2D1::RectF(x, y, x + w, y + h), pBrush);
    return Val_unit;
}

CAMLprim value guedra_select_font(value vsFamily, value vlSlant, value vlWeight, value vdSize) {
    wchar_t *family = convert_string(String_val(vsFamily));
    long slant_index = Long_val(vlSlant);
    long weight_index = Long_val(vlWeight);
    double font_size = Double_val(vdSize);

    DWRITE_FONT_WEIGHT fontWeight;
    DWRITE_FONT_STYLE fontStyle;

    if (slant_index == 0)
        fontStyle = DWRITE_FONT_STYLE_NORMAL;
    else if (slant_index == 1)
        fontStyle = DWRITE_FONT_STYLE_ITALIC;
    else
        fontStyle = DWRITE_FONT_STYLE_OBLIQUE;

    if (weight_index == 0)
        fontWeight = DWRITE_FONT_WEIGHT_NORMAL;
    else
        fontWeight = DWRITE_FONT_WEIGHT_BOLD;

    if (pTextFormat != NULL) {
        SafeRelease(&pTextFormat);
    }

    pDWriteFactory->CreateTextFormat
        (family,
         NULL,
         fontWeight,
         fontStyle,
         DWRITE_FONT_STRETCH_NORMAL,
         font_size,
         L"en-us",
         &pTextFormat);

    return Val_unit;
}

CAMLprim value guedra_show_text(value x, value y, value vsText) {
    wchar_t *text = convert_string(String_val(vsText));
    D2D1_RECT_F r = D2D1::RectF(Double_val(x), Double_val(y), 10000, 10000);
    pRT->DrawText(text, wcslen(text), pTextFormat, &r, pBrush);
    return Val_unit;
}

CAMLprim value push_clip(value x, value y, value w, value h) {
    D2D1_RECT_F r;
    r.left = Long_val(x);       // long -> float
    r.top = Long_val(y);
    r.right = r.left + Long_val(w);
    r.bottom = r.top + Long_val(h);
    /*  D2D1_ANTIALIAS_MODE_PER_PRIMITIVE
        D2D1_ANTIALIAS_MODE_ALIASED
        D2D1_ANTIALIAS_MODE_FORCE_DWORD */
    pRT->PushAxisAlignedClip(&r, D2D1_ANTIALIAS_MODE_PER_PRIMITIVE);
    return Val_unit;
}

CAMLprim value pop_clip(value u) {
    pRT->PopAxisAlignedClip();
    return Val_unit;
}

CAMLprim value guedra_push_translate(value vx, value vy)
{
    assert(transform_stack_index < TRANSFORM_STACK_LENGTH);
    double x = Double_val(vx);
    double y = Double_val(vy);
    D2D1::Matrix3x2F m;
    pRT->GetTransform(&m);
    transform_stack[transform_stack_index++] = m;
    m = D2D1::Matrix3x2F::Translation(x, y) * m;
    pRT->SetTransform(m);
    return Val_unit;
}

CAMLprim value guedra_push_translate_scale(value vx, value vy, value vscale)
{
    assert(transform_stack_index < TRANSFORM_STACK_LENGTH);
    double x = Double_val(vx);
    double y = Double_val(vy);
    double scale = Double_val(vscale);

    D2D_MATRIX_3X2_F c, m;
    pRT->GetTransform(&c);
    transform_stack[transform_stack_index++] = c;
    m._11 = m._22 = scale;
    m._12 = m._21 = 0.0;
    m._31 = x; m._32 = y;
    m = m * c;
    pRT->SetTransform(m);
    return Val_unit;
}

CAMLprim value guedra_pop_transform(value u)
{
    assert(transform_stack_index > 0);
    D2D_MATRIX_3X2_F m = transform_stack[--transform_stack_index];
    pRT->SetTransform(m);
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

    ID2D1PathGeometry *pPathGeometry;
    ID2D1GeometrySink *pSink;
    pD2DFactory->CreatePathGeometry(&pPathGeometry);
    pPathGeometry->Open(&pSink);
    pSink->BeginFigure(D2D1::Point2F(x, y), D2D1_FIGURE_BEGIN_FILLED);

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
        pSink->AddBezier
            (D2D1::BezierSegment
             (D2D1::Point2F(x0, y0),
              D2D1::Point2F(x1, y1),
              D2D1::Point2F(x2, y2)));
    }

    pSink->EndFigure(D2D1_FIGURE_END_CLOSED);
    pSink->Close();
    SafeRelease(&pSink);
    pRT->FillGeometry(pPathGeometry, pBrush);
    SafeRelease(&pPathGeometry);
    return Val_unit;
}

CAMLprim value guedra_text_extents(value vsText)
{
    CAMLparam1(vsText);
    CAMLlocal1(v);
    wchar_t *text = convert_string(String_val(vsText));
    IDWriteTextLayout *pTextLayout;
    assert(pTextFormat != NULL);
    HRESULT hr = pDWriteFactory->CreateTextLayout
        (text,
         wcslen(text),
         pTextFormat,
         10000,                 // The width of the layout box.
         10000,                 // The height of the layout box.
         &pTextLayout);
    DWRITE_TEXT_METRICS m;
    pTextLayout->GetMetrics(&m);
    DWRITE_LINE_METRICS lm;
    UINT32 s;
    pTextLayout->GetLineMetrics(&lm, 1, &s);
    v = caml_alloc(3, Double_array_tag);
    Store_double_field(v, 0, m.width);
    Store_double_field(v, 1, m.height);
    Store_double_field(v, 2, lm.baseline);
    CAMLreturn(v);
}

CAMLprim value delete_font(value vlFont_id)
{
    int font_id = Int_val(vlFont_id);
    res_font *font = (res_font *)res_get(RES_TYPE_FONT, font_id);
    res_free(RES_TYPE_FONT, font_id);
    if (pTextFormat == font->pTextFormat) {
        pTextFormat = NULL;
    }
    SafeRelease(&font->pTextFormat);
    free(font->family);
    free(font);
    return Val_unit;
}

CAMLprim value guedra_draw_text(value x, value y, value vsText)
{
    wchar_t *text = convert_string(String_val(vsText));
    D2D1_RECT_F r = D2D1::RectF(Double_val(x), Double_val(y), 10000, 10000);
    pRT->DrawText(text, wcslen(text), pTextFormat, &r, pBrush);
    return Val_unit;
}

CAMLprim value set_font(value vlFont_id)
{
    int font_id = Int_val(vlFont_id);
    res_font *font = (res_font *)res_get(RES_TYPE_FONT, font_id);
    pTextFormat = font->pTextFormat;
    return Val_unit;
}

CAMLprim value make_font(value vsFamily, value vlSlant, value vlWeight, value vdSize)
{
    res_font *font = (res_font *)malloc(sizeof(res_font));
    res_t *p = res_alloc(RES_TYPE_FONT, font, &font->id);

    font->family = clone_string(convert_string(String_val(vsFamily)));
    font->slant_index = Long_val(vlSlant);
    font->weight_index = Long_val(vlWeight);
    font->font_size = Double_val(vdSize);

    DWRITE_FONT_WEIGHT fontWeight;
    DWRITE_FONT_STYLE fontStyle;

    if (font->slant_index == 0)
        fontStyle = DWRITE_FONT_STYLE_NORMAL;
    else if (font->slant_index == 1)
        fontStyle = DWRITE_FONT_STYLE_ITALIC;
    else
        fontStyle = DWRITE_FONT_STYLE_OBLIQUE;

    if (font->weight_index == 0)
        fontWeight = DWRITE_FONT_WEIGHT_NORMAL;
    else
        fontWeight = DWRITE_FONT_WEIGHT_BOLD;

    pDWriteFactory->CreateTextFormat
        (font->family,
         NULL,
         fontWeight,
         fontStyle,
         DWRITE_FONT_STRETCH_NORMAL,
         font->font_size,
         L"en-us",
         &font->pTextFormat);

    return Val_long(font->id);
}

CAMLprim value guedra_set_dash(value va_dashes, value vf_offset)
{
    double offset = Double_val(vf_offset);
    int n = Wosize_val(va_dashes);
    float *dashes = (float *)_alloca(n * sizeof(float));
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

	SafeRelease(&strokeStyle);

	pD2DFactory->CreateStrokeStyle
		(D2D1::StrokeStyleProperties
		 (D2D1_CAP_STYLE_FLAT,
		  D2D1_CAP_STYLE_FLAT,
		  D2D1_CAP_STYLE_ROUND,
		  D2D1_LINE_JOIN_MITER,
		  10.0f,
		  D2D1_DASH_STYLE_CUSTOM,
		  0.0f),
		 dashes,
		 n,
		 &strokeStyle);

    return Val_unit;
}

CAMLprim value guedra_set_dash_symmetric(value vf_d, value vf_offset)
{
    double offset = Double_val(vf_offset);
    float dashes[2];
    dashes[0] = Double_val(vf_d);
    dashes[1] = Double_val(vf_d);

	SafeRelease(&strokeStyle);

	pD2DFactory->CreateStrokeStyle
		(D2D1::StrokeStyleProperties
		 (D2D1_CAP_STYLE_FLAT,
		  D2D1_CAP_STYLE_FLAT,
		  D2D1_CAP_STYLE_ROUND,
		  D2D1_LINE_JOIN_MITER,
		  10.0f,
		  D2D1_DASH_STYLE_CUSTOM,
		  0.0f),
		 dashes,
		 2,
		 &strokeStyle);

    return Val_unit;
}

CAMLprim value guedra_set_dash_solid(value v)
{
	SafeRelease(&strokeStyle);
    return Val_unit;
}

CAMLprim value imp_create_svg_surface(value vs_filename, value vf_width, value vf_height)
{
	pRT = bitmapRenderTarget;
    return Val_unit;
}

CAMLprim value imp_create_pdf_surface(value vs_filename, value vf_width, value vf_height)
{
	pRT = bitmapRenderTarget;
    return Val_unit;
}

CAMLprim value imp_delete_surface(value v)
{
	pRT = NULL;
    return Val_unit;
}

CAMLprim value imp_clipboard_set_text(value vs)
{
    char *s = String_val(vs);
    size_t n = caml_string_length(vs);
    return Val_unit;
}

CAMLprim value imp_guedra_quit(value vs)
{
	PostQuitMessage(0);
    return Val_unit;
}

} // end of extern "C"

ATOM MyRegisterClass(HINSTANCE hInstance) {
    WNDCLASSEX wcex;

    wcex.cbSize = sizeof(WNDCLASSEX);

    wcex.style          = CS_HREDRAW | CS_VREDRAW | CS_DBLCLKS;
    wcex.lpfnWndProc    = WndProc;
    wcex.cbClsExtra     = 0;
    wcex.cbWndExtra     = sizeof(LONG);     // extres window id
    wcex.hInstance      = hInstance;
    wcex.hIcon          = NULL;
    wcex.hCursor        = LoadCursor(nullptr, IDC_ARROW);
    wcex.hbrBackground  = (HBRUSH)(COLOR_WINDOW+1);
    wcex.lpszMenuName   = NULL;
    wcex.lpszClassName  = L"GUEDRA";
    wcex.hIconSm        = NULL;

    return RegisterClassEx(&wcex);
}

int wmain(int argc, wchar_t **argv)
{
    HINSTANCE hInstance = GetModuleHandle(NULL);
    HRESULT hr = D2D1CreateFactory(D2D1_FACTORY_TYPE_SINGLE_THREADED,
                                   &pD2DFactory);
    hr = DWriteCreateFactory(DWRITE_FACTORY_TYPE_SHARED,
                             __uuidof(IDWriteFactory),
                             reinterpret_cast<IUnknown**>(&pDWriteFactory));
    MyRegisterClass(hInstance);
    res_book_init();
    caml_main(argv);
    guedra_init();

    MSG msg;
    while (GetMessage(&msg, nullptr, 0, 0))
    {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }
    return 0;
}
