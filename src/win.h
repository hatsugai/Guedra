#ifdef __cplusplus
extern "C" {
#endif

enum {
    Tag_EndSession,
    Tag_Timer,
    Tag_WinClose,
    Tag_MouseEnter,
    Tag_MouseLeave,
};

enum {
    Tag_WinSize,
    Tag_Paint,
    Tag_MouseDown,
    Tag_MouseUp,
    Tag_MouseMove,
    Tag_MouseWheel,
    Tag_KeyDown,
    Tag_KeyUp,
    Tag_Char,
    Tag_Active,
    Tag_Scroll,
    Tag_Scale,
};

#define mouseButtonLeft              1
#define mouseButtonRight             2
#define mouseButtonMiddle            4
#define mouseButtonStateDoubleClick  8
#define keyStateShift                16
#define keyStateControl              32
#define keyStateAlt                  64
#define keyStateMeta                 128
#define keyStateRepeat               256
#define keyBackSpace                 65288
#define keyTab                       65289
#define keyReturn                    65293
#define keyEscape                    65307
#define keyDelete                    65535
#define keyHome                      65360
#define keyLeft                      65361
#define keyUp                        65362
#define keyRight                     65363
#define keyDown                      65364
#define keyPageUp                    65365
#define keyPageDown                  65366
#define keyEnd                       65367
#define keyInsert                    65379
#define keyF1                        65470
#define keyF2                        65471
#define keyF3                        65472
#define keyF4                        65473
#define keyF5                        65474
#define keyF6                        65475
#define keyF7                        65476
#define keyF8                        65477
#define keyF9                        65478
#define keyF10                       65479
#define keyF11                       65480
#define keyF12                       65481
#define keyShift                     65505
#define keyControl                   65507
#define keyMeta                      65511
#define keyAlt                       65513
#define mouseCursorProgress          0
#define mouseCursorArrow             1
#define mouseCursorCross             2
#define mouseCursorHand              3
#define mouseCursorHelp              4
#define mouseCursorText              5
#define mouseCursorNo                6
#define mouseCursorMove              7
#define mouseCursorNWSE              8
#define mouseCursorNESW              9
#define mouseCursorNS                10
#define mouseCursorWE                11
#define mouseCursorWait              12
#define fontSlantNormal              0
#define fontSlantItalic              1
#define fontSlantOblique             2
#define fontWeightNormal             0
#define fontWeightBold               1
#define arcClockwise                 1
#define arcCounterClockwise          0
#define arcSizeSmall                 1
#define arcSizeLarge                 0

void guedra_init(void);
void notify_end_session(void);
void notify_timer(void);
void notify_win_size(unsigned win_id, int w, int h);
void notify_win_close(unsigned win_id);
void notify_paint(unsigned win_id, int x, int y, int w, int h);
void notify_mouse_enter(unsigned win_id);
void notify_mouse_leave(unsigned win_id);
void notify_mouse_down(unsigned win_id, int x, int y, unsigned state, unsigned button);
void notify_mouse_up(unsigned win_id, int x, int y, unsigned state, unsigned button);
void notify_mouse_move(unsigned win_id, int x, int y, unsigned state);
void notify_mouse_wheel(unsigned win_id, int x, int y, unsigned state, int direction);
void notify_scroll(unsigned win_id, int x, int y, unsigned state, int dx, int dy);
void notify_scale(unsigned win_id, double scale);
void notify_key_down(unsigned win_id, int keycode, unsigned state);
void notify_key_up(unsigned win_id, int keycode, unsigned state);
void notify_char(unsigned win_id, int c);

#ifdef __cplusplus
}
#endif
