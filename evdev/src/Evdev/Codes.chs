{-
TODO haddock doesn't quite work correctly with LINE pragmas
    https://github.com/haskell/haddock/issues/441
    for now we can work around this by deleting the pragmas before upload to hackage

seems to be on its way to being fixed with `.hie` files (enable `-fwrite-ide-info`)
    https://github.com/haskell/haddock/commit/8bc3c2990475a254e168fbdb005af93f9397b19c
-}

-- | Datatypes corresponding to the constants in [input-event-codes.h](https://github.com/torvalds/linux/blob/master/include/uapi/linux/input-event-codes.h).
-- See [the Linux Kernel documentation](https://www.kernel.org/doc/html/latest/input/event-codes.html) for full details, noting that all names have been mechanically transformed into CamelCase.
module Evdev.Codes
    ( EventType(..)
    , SyncEvent(..)
    , Key
        ( ..
        , KeyHanguel
        , KeyCoffee
        , KeyDirection
        , KeyBrightnessZero
        , KeyWimax
        , BtnMisc
        , BtnMouse
        , BtnTrigger
        , BtnGamepad
        , BtnSouth
        , BtnEast
        , BtnNorth
        , BtnWest
        , BtnDigi
        , BtnWheel
        , KeyBrightnessToggle
        , BtnTriggerHappy )
    , RelativeAxis(..)
    , AbsoluteAxis(..)
    , SwitchEvent(..)
    , MiscEvent(..)
    , LEDEvent(..)
    , RepeatEvent(..)
    , SoundEvent(..)
    , DeviceProperty(..)
    ) where

#include <linux/input-event-codes.h>

-- | Each of these corresponds to one of the contructors of 'Evdev.EventData'. So you're unlikely to need to use these directly (C doesn't have ADTs - we do).
{#enum define EventType {
    EV_SYN as EvSyn,
    EV_KEY as EvKey,
    EV_REL as EvRel,
    EV_ABS as EvAbs,
    EV_MSC as EvMsc,
    EV_SW as EvSw,
    EV_LED as EvLed,
    EV_SND as EvSnd,
    EV_REP as EvRep,
    EV_FF as EvFf,
    EV_PWR as EvPwr,
    EV_FF_STATUS as EvFfStatus}
    deriving (Bounded, Eq, Ord, Read, Show) #}

-- | Synchronization events
{#enum define SyncEvent {
    SYN_REPORT as SynReport, -- | Used to separate packets of simultaneous events
    SYN_CONFIG as SynConfig,
    SYN_MT_REPORT as SynMtReport,
    SYN_DROPPED as SynDropped} --TODO handle SYN_DROPPED automatically for streams
    deriving (Bounded, Eq, Ord, Read, Show) #}

-- | Keys and buttons
{#enum define Key {
    KEY_RESERVED as KeyReserved,
    KEY_ESC as KeyEsc,
    KEY_1 as Key1,
    KEY_2 as Key2,
    KEY_3 as Key3,
    KEY_4 as Key4,
    KEY_5 as Key5,
    KEY_6 as Key6,
    KEY_7 as Key7,
    KEY_8 as Key8,
    KEY_9 as Key9,
    KEY_0 as Key0,
    KEY_MINUS as KeyMinus,
    KEY_EQUAL as KeyEqual,
    KEY_BACKSPACE as KeyBackspace,
    KEY_TAB as KeyTab,
    KEY_Q as KeyQ,
    KEY_W as KeyW,
    KEY_E as KeyE,
    KEY_R as KeyR,
    KEY_T as KeyT,
    KEY_Y as KeyY,
    KEY_U as KeyU,
    KEY_I as KeyI,
    KEY_O as KeyO,
    KEY_P as KeyP,
    KEY_LEFTBRACE as KeyLeftbrace,
    KEY_RIGHTBRACE as KeyRightbrace,
    KEY_ENTER as KeyEnter,
    KEY_LEFTCTRL as KeyLeftctrl,
    KEY_A as KeyA,
    KEY_S as KeyS,
    KEY_D as KeyD,
    KEY_F as KeyF,
    KEY_G as KeyG,
    KEY_H as KeyH,
    KEY_J as KeyJ,
    KEY_K as KeyK,
    KEY_L as KeyL,
    KEY_SEMICOLON as KeySemicolon,
    KEY_APOSTROPHE as KeyApostrophe,
    KEY_GRAVE as KeyGrave,
    KEY_LEFTSHIFT as KeyLeftshift,
    KEY_BACKSLASH as KeyBackslash,
    KEY_Z as KeyZ,
    KEY_X as KeyX,
    KEY_C as KeyC,
    KEY_V as KeyV,
    KEY_B as KeyB,
    KEY_N as KeyN,
    KEY_M as KeyM,
    KEY_COMMA as KeyComma,
    KEY_DOT as KeyDot,
    KEY_SLASH as KeySlash,
    KEY_RIGHTSHIFT as KeyRightshift,
    KEY_KPASTERISK as KeyKpasterisk,
    KEY_LEFTALT as KeyLeftalt,
    KEY_SPACE as KeySpace,
    KEY_CAPSLOCK as KeyCapslock,
    KEY_F1 as KeyF1,
    KEY_F2 as KeyF2,
    KEY_F3 as KeyF3,
    KEY_F4 as KeyF4,
    KEY_F5 as KeyF5,
    KEY_F6 as KeyF6,
    KEY_F7 as KeyF7,
    KEY_F8 as KeyF8,
    KEY_F9 as KeyF9,
    KEY_F10 as KeyF10,
    KEY_NUMLOCK as KeyNumlock,
    KEY_SCROLLLOCK as KeyScrolllock,
    KEY_KP7 as KeyKp7,
    KEY_KP8 as KeyKp8,
    KEY_KP9 as KeyKp9,
    KEY_KPMINUS as KeyKpminus,
    KEY_KP4 as KeyKp4,
    KEY_KP5 as KeyKp5,
    KEY_KP6 as KeyKp6,
    KEY_KPPLUS as KeyKpplus,
    KEY_KP1 as KeyKp1,
    KEY_KP2 as KeyKp2,
    KEY_KP3 as KeyKp3,
    KEY_KP0 as KeyKp0,
    KEY_KPDOT as KeyKpdot,
    KEY_ZENKAKUHANKAKU as KeyZenkakuhankaku,
    KEY_102ND as Key102nd,
    KEY_F11 as KeyF11,
    KEY_F12 as KeyF12,
    KEY_RO as KeyRo,
    KEY_KATAKANA as KeyKatakana,
    KEY_HIRAGANA as KeyHiragana,
    KEY_HENKAN as KeyHenkan,
    KEY_KATAKANAHIRAGANA as KeyKatakanahiragana,
    KEY_MUHENKAN as KeyMuhenkan,
    KEY_KPJPCOMMA as KeyKpjpcomma,
    KEY_KPENTER as KeyKpenter,
    KEY_RIGHTCTRL as KeyRightctrl,
    KEY_KPSLASH as KeyKpslash,
    KEY_SYSRQ as KeySysrq,
    KEY_RIGHTALT as KeyRightalt,
    KEY_LINEFEED as KeyLinefeed,
    KEY_HOME as KeyHome,
    KEY_UP as KeyUp,
    KEY_PAGEUP as KeyPageup,
    KEY_LEFT as KeyLeft,
    KEY_RIGHT as KeyRight,
    KEY_END as KeyEnd,
    KEY_DOWN as KeyDown,
    KEY_PAGEDOWN as KeyPagedown,
    KEY_INSERT as KeyInsert,
    KEY_DELETE as KeyDelete,
    KEY_MACRO as KeyMacro,
    KEY_MUTE as KeyMute,
    KEY_VOLUMEDOWN as KeyVolumedown,
    KEY_VOLUMEUP as KeyVolumeup,
    KEY_POWER as KeyPower,
    KEY_KPEQUAL as KeyKpequal,
    KEY_KPPLUSMINUS as KeyKpplusminus,
    KEY_PAUSE as KeyPause,
    KEY_SCALE as KeyScale,
    KEY_KPCOMMA as KeyKpcomma,
    KEY_HANGEUL as KeyHangeul,
    -- KEY_HANGUEL as KeyHanguel, (alias of KEY_HANGEUL)
    KEY_HANJA as KeyHanja,
    KEY_YEN as KeyYen,
    KEY_LEFTMETA as KeyLeftmeta,
    KEY_RIGHTMETA as KeyRightmeta,
    KEY_COMPOSE as KeyCompose,
    KEY_STOP as KeyStop,
    KEY_AGAIN as KeyAgain,
    KEY_PROPS as KeyProps,
    KEY_UNDO as KeyUndo,
    KEY_FRONT as KeyFront,
    KEY_COPY as KeyCopy,
    KEY_OPEN as KeyOpen,
    KEY_PASTE as KeyPaste,
    KEY_FIND as KeyFind,
    KEY_CUT as KeyCut,
    KEY_HELP as KeyHelp,
    KEY_MENU as KeyMenu,
    KEY_CALC as KeyCalc,
    KEY_SETUP as KeySetup,
    KEY_SLEEP as KeySleep,
    KEY_WAKEUP as KeyWakeup,
    KEY_FILE as KeyFile,
    KEY_SENDFILE as KeySendfile,
    KEY_DELETEFILE as KeyDeletefile,
    KEY_XFER as KeyXfer,
    KEY_PROG1 as KeyProg1,
    KEY_PROG2 as KeyProg2,
    KEY_WWW as KeyWww,
    KEY_MSDOS as KeyMsdos,
    -- KEY_COFFEE as KeyCoffee, (alias of KEY_SCREENLOCK)
    KEY_SCREENLOCK as KeyScreenlock,
    KEY_ROTATE_DISPLAY as KeyRotateDisplay,
    -- KEY_DIRECTION as KeyDirection, (alias of KEY_ROTATE_DISPLAY)
    KEY_CYCLEWINDOWS as KeyCyclewindows,
    KEY_MAIL as KeyMail,
    KEY_BOOKMARKS as KeyBookmarks,
    KEY_COMPUTER as KeyComputer,
    KEY_BACK as KeyBack,
    KEY_FORWARD as KeyForward,
    KEY_CLOSECD as KeyClosecd,
    KEY_EJECTCD as KeyEjectcd,
    KEY_EJECTCLOSECD as KeyEjectclosecd,
    KEY_NEXTSONG as KeyNextsong,
    KEY_PLAYPAUSE as KeyPlaypause,
    KEY_PREVIOUSSONG as KeyPrevioussong,
    KEY_STOPCD as KeyStopcd,
    KEY_RECORD as KeyRecord,
    KEY_REWIND as KeyRewind,
    KEY_PHONE as KeyPhone,
    KEY_ISO as KeyIso,
    KEY_CONFIG as KeyConfig,
    KEY_HOMEPAGE as KeyHomepage,
    KEY_REFRESH as KeyRefresh,
    KEY_EXIT as KeyExit,
    KEY_MOVE as KeyMove,
    KEY_EDIT as KeyEdit,
    KEY_SCROLLUP as KeyScrollup,
    KEY_SCROLLDOWN as KeyScrolldown,
    KEY_KPLEFTPAREN as KeyKpleftparen,
    KEY_KPRIGHTPAREN as KeyKprightparen,
    KEY_NEW as KeyNew,
    KEY_REDO as KeyRedo,
    KEY_F13 as KeyF13,
    KEY_F14 as KeyF14,
    KEY_F15 as KeyF15,
    KEY_F16 as KeyF16,
    KEY_F17 as KeyF17,
    KEY_F18 as KeyF18,
    KEY_F19 as KeyF19,
    KEY_F20 as KeyF20,
    KEY_F21 as KeyF21,
    KEY_F22 as KeyF22,
    KEY_F23 as KeyF23,
    KEY_F24 as KeyF24,
    KEY_PLAYCD as KeyPlaycd,
    KEY_PAUSECD as KeyPausecd,
    KEY_PROG3 as KeyProg3,
    KEY_PROG4 as KeyProg4,
    KEY_DASHBOARD as KeyDashboard,
    KEY_SUSPEND as KeySuspend,
    KEY_CLOSE as KeyClose,
    KEY_PLAY as KeyPlay,
    KEY_FASTFORWARD as KeyFastforward,
    KEY_BASSBOOST as KeyBassboost,
    KEY_PRINT as KeyPrint,
    KEY_HP as KeyHp,
    KEY_CAMERA as KeyCamera,
    KEY_SOUND as KeySound,
    KEY_QUESTION as KeyQuestion,
    KEY_EMAIL as KeyEmail,
    KEY_CHAT as KeyChat,
    KEY_SEARCH as KeySearch,
    KEY_CONNECT as KeyConnect,
    KEY_FINANCE as KeyFinance,
    KEY_SPORT as KeySport,
    KEY_SHOP as KeyShop,
    KEY_ALTERASE as KeyAlterase,
    KEY_CANCEL as KeyCancel,
    KEY_BRIGHTNESSDOWN as KeyBrightnessdown,
    KEY_BRIGHTNESSUP as KeyBrightnessup,
    KEY_MEDIA as KeyMedia,
    KEY_SWITCHVIDEOMODE as KeySwitchvideomode,
    KEY_KBDILLUMTOGGLE as KeyKbdillumtoggle,
    KEY_KBDILLUMDOWN as KeyKbdillumdown,
    KEY_KBDILLUMUP as KeyKbdillumup,
    KEY_SEND as KeySend,
    KEY_REPLY as KeyReply,
    KEY_FORWARDMAIL as KeyForwardmail,
    KEY_SAVE as KeySave,
    KEY_DOCUMENTS as KeyDocuments,
    KEY_BATTERY as KeyBattery,
    KEY_BLUETOOTH as KeyBluetooth,
    KEY_WLAN as KeyWlan,
    KEY_UWB as KeyUwb,
    KEY_UNKNOWN as KeyUnknown,
    KEY_VIDEO_NEXT as KeyVideoNext,
    KEY_VIDEO_PREV as KeyVideoPrev,
    KEY_BRIGHTNESS_CYCLE as KeyBrightnessCycle,
    KEY_BRIGHTNESS_AUTO as KeyBrightnessAuto,
    -- KEY_BRIGHTNESS_ZERO as KeyBrightnessZero, (alias of KEY_BRIGHTNESS_AUTO)
    KEY_DISPLAY_OFF as KeyDisplayOff,
    KEY_WWAN as KeyWwan,
    -- KEY_WIMAX as KeyWimax, (alias of KEY_WWAN)
    KEY_RFKILL as KeyRfkill,
    KEY_MICMUTE as KeyMicmute,
    -- BTN_MISC as BtnMisc, (alias of BTN_0)
    BTN_0 as Btn0,
    BTN_1 as Btn1,
    BTN_2 as Btn2,
    BTN_3 as Btn3,
    BTN_4 as Btn4,
    BTN_5 as Btn5,
    BTN_6 as Btn6,
    BTN_7 as Btn7,
    BTN_8 as Btn8,
    BTN_9 as Btn9,
    -- BTN_MOUSE as BtnMouse, (alias of BTN_LEFT)
    BTN_LEFT as BtnLeft,
    BTN_RIGHT as BtnRight,
    BTN_MIDDLE as BtnMiddle,
    BTN_SIDE as BtnSide,
    BTN_EXTRA as BtnExtra,
    BTN_FORWARD as BtnForward,
    BTN_BACK as BtnBack,
    BTN_TASK as BtnTask,
    BTN_JOYSTICK as BtnJoystick,
    -- BTN_TRIGGER as BtnTrigger, (alias of BTN_JOYSTICK)
    BTN_THUMB as BtnThumb,
    BTN_THUMB2 as BtnThumb2,
    BTN_TOP as BtnTop,
    BTN_TOP2 as BtnTop2,
    BTN_PINKIE as BtnPinkie,
    BTN_BASE as BtnBase,
    BTN_BASE2 as BtnBase2,
    BTN_BASE3 as BtnBase3,
    BTN_BASE4 as BtnBase4,
    BTN_BASE5 as BtnBase5,
    BTN_BASE6 as BtnBase6,
    BTN_DEAD as BtnDead,
    -- BTN_GAMEPAD as BtnGamepad, (alias of BTN_A)
    -- BTN_SOUTH as BtnSouth, (alias of BTN_A)
    BTN_A as BtnA,
    -- BTN_EAST as BtnEast, (alias of BTN_B)
    BTN_B as BtnB,
    BTN_C as BtnC,
    -- BTN_NORTH as BtnNorth, (alias of BTN_X)
    BTN_X as BtnX,
    -- BTN_WEST as BtnWest, (alias of BTN_Y)
    BTN_Y as BtnY,
    BTN_Z as BtnZ,
    BTN_TL as BtnTl,
    BTN_TR as BtnTr,
    BTN_TL2 as BtnTl2,
    BTN_TR2 as BtnTr2,
    BTN_SELECT as BtnSelect,
    BTN_START as BtnStart,
    BTN_MODE as BtnMode,
    BTN_THUMBL as BtnThumbl,
    BTN_THUMBR as BtnThumbr,
    -- BTN_DIGI as BtnDigi, (alias of BTN_TOOL_PEN)
    BTN_TOOL_PEN as BtnToolPen,
    BTN_TOOL_RUBBER as BtnToolRubber,
    BTN_TOOL_BRUSH as BtnToolBrush,
    BTN_TOOL_PENCIL as BtnToolPencil,
    BTN_TOOL_AIRBRUSH as BtnToolAirbrush,
    BTN_TOOL_FINGER as BtnToolFinger,
    BTN_TOOL_MOUSE as BtnToolMouse,
    BTN_TOOL_LENS as BtnToolLens,
    BTN_TOOL_QUINTTAP as BtnToolQuinttap,
    BTN_TOUCH as BtnTouch,
    BTN_STYLUS as BtnStylus,
    BTN_STYLUS2 as BtnStylus2,
    BTN_TOOL_DOUBLETAP as BtnToolDoubletap,
    BTN_TOOL_TRIPLETAP as BtnToolTripletap,
    BTN_TOOL_QUADTAP as BtnToolQuadtap,
    -- BTN_WHEEL as BtnWheel, (alias of BTN_GEAR_DOWN)
    BTN_GEAR_DOWN as BtnGearDown,
    BTN_GEAR_UP as BtnGearUp,
    KEY_OK as KeyOk,
    KEY_SELECT as KeySelect,
    KEY_GOTO as KeyGoto,
    KEY_CLEAR as KeyClear,
    KEY_POWER2 as KeyPower2,
    KEY_OPTION as KeyOption,
    KEY_INFO as KeyInfo,
    KEY_TIME as KeyTime,
    KEY_VENDOR as KeyVendor,
    KEY_ARCHIVE as KeyArchive,
    KEY_PROGRAM as KeyProgram,
    KEY_CHANNEL as KeyChannel,
    KEY_FAVORITES as KeyFavorites,
    KEY_EPG as KeyEpg,
    KEY_PVR as KeyPvr,
    KEY_MHP as KeyMhp,
    KEY_LANGUAGE as KeyLanguage,
    KEY_TITLE as KeyTitle,
    KEY_SUBTITLE as KeySubtitle,
    KEY_ANGLE as KeyAngle,
    KEY_ZOOM as KeyZoom,
    KEY_MODE as KeyMode,
    KEY_KEYBOARD as KeyKeyboard,
    KEY_SCREEN as KeyScreen,
    KEY_PC as KeyPc,
    KEY_TV as KeyTv,
    KEY_TV2 as KeyTv2,
    KEY_VCR as KeyVcr,
    KEY_VCR2 as KeyVcr2,
    KEY_SAT as KeySat,
    KEY_SAT2 as KeySat2,
    KEY_CD as KeyCd,
    KEY_TAPE as KeyTape,
    KEY_RADIO as KeyRadio,
    KEY_TUNER as KeyTuner,
    KEY_PLAYER as KeyPlayer,
    KEY_TEXT as KeyText,
    KEY_DVD as KeyDvd,
    KEY_AUX as KeyAux,
    KEY_MP3 as KeyMp3,
    KEY_AUDIO as KeyAudio,
    KEY_VIDEO as KeyVideo,
    KEY_DIRECTORY as KeyDirectory,
    KEY_LIST as KeyList,
    KEY_MEMO as KeyMemo,
    KEY_CALENDAR as KeyCalendar,
    KEY_RED as KeyRed,
    KEY_GREEN as KeyGreen,
    KEY_YELLOW as KeyYellow,
    KEY_BLUE as KeyBlue,
    KEY_CHANNELUP as KeyChannelup,
    KEY_CHANNELDOWN as KeyChanneldown,
    KEY_FIRST as KeyFirst,
    KEY_LAST as KeyLast,
    KEY_AB as KeyAb,
    KEY_NEXT as KeyNext,
    KEY_RESTART as KeyRestart,
    KEY_SLOW as KeySlow,
    KEY_SHUFFLE as KeyShuffle,
    KEY_BREAK as KeyBreak,
    KEY_PREVIOUS as KeyPrevious,
    KEY_DIGITS as KeyDigits,
    KEY_TEEN as KeyTeen,
    KEY_TWEN as KeyTwen,
    KEY_VIDEOPHONE as KeyVideophone,
    KEY_GAMES as KeyGames,
    KEY_ZOOMIN as KeyZoomin,
    KEY_ZOOMOUT as KeyZoomout,
    KEY_ZOOMRESET as KeyZoomreset,
    KEY_WORDPROCESSOR as KeyWordprocessor,
    KEY_EDITOR as KeyEditor,
    KEY_SPREADSHEET as KeySpreadsheet,
    KEY_GRAPHICSEDITOR as KeyGraphicseditor,
    KEY_PRESENTATION as KeyPresentation,
    KEY_DATABASE as KeyDatabase,
    KEY_NEWS as KeyNews,
    KEY_VOICEMAIL as KeyVoicemail,
    KEY_ADDRESSBOOK as KeyAddressbook,
    KEY_MESSENGER as KeyMessenger,
    KEY_DISPLAYTOGGLE as KeyDisplaytoggle,
    -- KEY_BRIGHTNESS_TOGGLE as KeyBrightnessToggle, (alias of KEY_DISPLAYTOGGLE)
    KEY_SPELLCHECK as KeySpellcheck,
    KEY_LOGOFF as KeyLogoff,
    KEY_DOLLAR as KeyDollar,
    KEY_EURO as KeyEuro,
    KEY_FRAMEBACK as KeyFrameback,
    KEY_FRAMEFORWARD as KeyFrameforward,
    KEY_CONTEXT_MENU as KeyContextMenu,
    KEY_MEDIA_REPEAT as KeyMediaRepeat,
    KEY_10CHANNELSUP as Key10channelsup,
    KEY_10CHANNELSDOWN as Key10channelsdown,
    KEY_IMAGES as KeyImages,
    KEY_DEL_EOL as KeyDelEol,
    KEY_DEL_EOS as KeyDelEos,
    KEY_INS_LINE as KeyInsLine,
    KEY_DEL_LINE as KeyDelLine,
    KEY_FN as KeyFn,
    KEY_FN_ESC as KeyFnEsc,
    KEY_FN_F1 as KeyFnF1,
    KEY_FN_F2 as KeyFnF2,
    KEY_FN_F3 as KeyFnF3,
    KEY_FN_F4 as KeyFnF4,
    KEY_FN_F5 as KeyFnF5,
    KEY_FN_F6 as KeyFnF6,
    KEY_FN_F7 as KeyFnF7,
    KEY_FN_F8 as KeyFnF8,
    KEY_FN_F9 as KeyFnF9,
    KEY_FN_F10 as KeyFnF10,
    KEY_FN_F11 as KeyFnF11,
    KEY_FN_F12 as KeyFnF12,
    KEY_FN_1 as KeyFn1,
    KEY_FN_2 as KeyFn2,
    KEY_FN_D as KeyFnD,
    KEY_FN_E as KeyFnE,
    KEY_FN_F as KeyFnF,
    KEY_FN_S as KeyFnS,
    KEY_FN_B as KeyFnB,
    KEY_BRL_DOT1 as KeyBrlDot1,
    KEY_BRL_DOT2 as KeyBrlDot2,
    KEY_BRL_DOT3 as KeyBrlDot3,
    KEY_BRL_DOT4 as KeyBrlDot4,
    KEY_BRL_DOT5 as KeyBrlDot5,
    KEY_BRL_DOT6 as KeyBrlDot6,
    KEY_BRL_DOT7 as KeyBrlDot7,
    KEY_BRL_DOT8 as KeyBrlDot8,
    KEY_BRL_DOT9 as KeyBrlDot9,
    KEY_BRL_DOT10 as KeyBrlDot10,
    KEY_NUMERIC_0 as KeyNumeric0,
    KEY_NUMERIC_1 as KeyNumeric1,
    KEY_NUMERIC_2 as KeyNumeric2,
    KEY_NUMERIC_3 as KeyNumeric3,
    KEY_NUMERIC_4 as KeyNumeric4,
    KEY_NUMERIC_5 as KeyNumeric5,
    KEY_NUMERIC_6 as KeyNumeric6,
    KEY_NUMERIC_7 as KeyNumeric7,
    KEY_NUMERIC_8 as KeyNumeric8,
    KEY_NUMERIC_9 as KeyNumeric9,
    KEY_NUMERIC_STAR as KeyNumericStar,
    KEY_NUMERIC_POUND as KeyNumericPound,
    KEY_NUMERIC_A as KeyNumericA,
    KEY_NUMERIC_B as KeyNumericB,
    KEY_NUMERIC_C as KeyNumericC,
    KEY_NUMERIC_D as KeyNumericD,
    KEY_CAMERA_FOCUS as KeyCameraFocus,
    KEY_WPS_BUTTON as KeyWpsButton,
    KEY_TOUCHPAD_TOGGLE as KeyTouchpadToggle,
    KEY_TOUCHPAD_ON as KeyTouchpadOn,
    KEY_TOUCHPAD_OFF as KeyTouchpadOff,
    KEY_CAMERA_ZOOMIN as KeyCameraZoomin,
    KEY_CAMERA_ZOOMOUT as KeyCameraZoomout,
    KEY_CAMERA_UP as KeyCameraUp,
    KEY_CAMERA_DOWN as KeyCameraDown,
    KEY_CAMERA_LEFT as KeyCameraLeft,
    KEY_CAMERA_RIGHT as KeyCameraRight,
    KEY_ATTENDANT_ON as KeyAttendantOn,
    KEY_ATTENDANT_OFF as KeyAttendantOff,
    KEY_ATTENDANT_TOGGLE as KeyAttendantToggle,
    KEY_LIGHTS_TOGGLE as KeyLightsToggle,
    BTN_DPAD_UP as BtnDpadUp,
    BTN_DPAD_DOWN as BtnDpadDown,
    BTN_DPAD_LEFT as BtnDpadLeft,
    BTN_DPAD_RIGHT as BtnDpadRight,
    KEY_ALS_TOGGLE as KeyAlsToggle,
    KEY_BUTTONCONFIG as KeyButtonconfig,
    KEY_TASKMANAGER as KeyTaskmanager,
    KEY_JOURNAL as KeyJournal,
    KEY_CONTROLPANEL as KeyControlpanel,
    KEY_APPSELECT as KeyAppselect,
    KEY_SCREENSAVER as KeyScreensaver,
    KEY_VOICECOMMAND as KeyVoicecommand,
    KEY_BRIGHTNESS_MIN as KeyBrightnessMin,
    KEY_BRIGHTNESS_MAX as KeyBrightnessMax,
    KEY_KBDINPUTASSIST_PREV as KeyKbdinputassistPrev,
    KEY_KBDINPUTASSIST_NEXT as KeyKbdinputassistNext,
    KEY_KBDINPUTASSIST_PREVGROUP as KeyKbdinputassistPrevgroup,
    KEY_KBDINPUTASSIST_NEXTGROUP as KeyKbdinputassistNextgroup,
    KEY_KBDINPUTASSIST_ACCEPT as KeyKbdinputassistAccept,
    KEY_KBDINPUTASSIST_CANCEL as KeyKbdinputassistCancel,
    -- BTN_TRIGGER_HAPPY as BtnTriggerHappy, (alias of BTN_TRIGGER_HAPPY1)
    BTN_TRIGGER_HAPPY1 as BtnTriggerHappy1,
    BTN_TRIGGER_HAPPY2 as BtnTriggerHappy2,
    BTN_TRIGGER_HAPPY3 as BtnTriggerHappy3,
    BTN_TRIGGER_HAPPY4 as BtnTriggerHappy4,
    BTN_TRIGGER_HAPPY5 as BtnTriggerHappy5,
    BTN_TRIGGER_HAPPY6 as BtnTriggerHappy6,
    BTN_TRIGGER_HAPPY7 as BtnTriggerHappy7,
    BTN_TRIGGER_HAPPY8 as BtnTriggerHappy8,
    BTN_TRIGGER_HAPPY9 as BtnTriggerHappy9,
    BTN_TRIGGER_HAPPY10 as BtnTriggerHappy10,
    BTN_TRIGGER_HAPPY11 as BtnTriggerHappy11,
    BTN_TRIGGER_HAPPY12 as BtnTriggerHappy12,
    BTN_TRIGGER_HAPPY13 as BtnTriggerHappy13,
    BTN_TRIGGER_HAPPY14 as BtnTriggerHappy14,
    BTN_TRIGGER_HAPPY15 as BtnTriggerHappy15,
    BTN_TRIGGER_HAPPY16 as BtnTriggerHappy16,
    BTN_TRIGGER_HAPPY17 as BtnTriggerHappy17,
    BTN_TRIGGER_HAPPY18 as BtnTriggerHappy18,
    BTN_TRIGGER_HAPPY19 as BtnTriggerHappy19,
    BTN_TRIGGER_HAPPY20 as BtnTriggerHappy20,
    BTN_TRIGGER_HAPPY21 as BtnTriggerHappy21,
    BTN_TRIGGER_HAPPY22 as BtnTriggerHappy22,
    BTN_TRIGGER_HAPPY23 as BtnTriggerHappy23,
    BTN_TRIGGER_HAPPY24 as BtnTriggerHappy24,
    BTN_TRIGGER_HAPPY25 as BtnTriggerHappy25,
    BTN_TRIGGER_HAPPY26 as BtnTriggerHappy26,
    BTN_TRIGGER_HAPPY27 as BtnTriggerHappy27,
    BTN_TRIGGER_HAPPY28 as BtnTriggerHappy28,
    BTN_TRIGGER_HAPPY29 as BtnTriggerHappy29,
    BTN_TRIGGER_HAPPY30 as BtnTriggerHappy30,
    BTN_TRIGGER_HAPPY31 as BtnTriggerHappy31,
    BTN_TRIGGER_HAPPY32 as BtnTriggerHappy32,
    BTN_TRIGGER_HAPPY33 as BtnTriggerHappy33,
    BTN_TRIGGER_HAPPY34 as BtnTriggerHappy34,
    BTN_TRIGGER_HAPPY35 as BtnTriggerHappy35,
    BTN_TRIGGER_HAPPY36 as BtnTriggerHappy36,
    BTN_TRIGGER_HAPPY37 as BtnTriggerHappy37,
    BTN_TRIGGER_HAPPY38 as BtnTriggerHappy38,
    BTN_TRIGGER_HAPPY39 as BtnTriggerHappy39,
    BTN_TRIGGER_HAPPY40 as BtnTriggerHappy40}
    deriving (Bounded, Eq, Ord, Read, Show) #}

pattern KeyHanguel :: Key
pattern KeyHanguel = KeyHangeul

pattern KeyCoffee :: Key
pattern KeyCoffee = KeyScreenlock

pattern KeyDirection :: Key
pattern KeyDirection = KeyRotateDisplay

pattern KeyBrightnessZero :: Key
pattern KeyBrightnessZero = KeyBrightnessAuto

pattern KeyWimax :: Key
pattern KeyWimax = KeyWwan

pattern BtnMisc :: Key
pattern BtnMisc = Btn0

pattern BtnMouse :: Key
pattern BtnMouse = BtnLeft

pattern BtnTrigger :: Key
pattern BtnTrigger = BtnJoystick

pattern BtnGamepad :: Key
pattern BtnGamepad = BtnA

pattern BtnSouth :: Key
pattern BtnSouth = BtnA

pattern BtnEast :: Key
pattern BtnEast = BtnB

pattern BtnNorth :: Key
pattern BtnNorth = BtnX

pattern BtnWest :: Key
pattern BtnWest = BtnY

pattern BtnDigi :: Key
pattern BtnDigi = BtnToolPen

pattern BtnWheel :: Key
pattern BtnWheel = BtnGearDown

pattern KeyBrightnessToggle :: Key
pattern KeyBrightnessToggle = KeyDisplaytoggle

pattern BtnTriggerHappy :: Key
pattern BtnTriggerHappy = BtnTriggerHappy1

-- | Relative changes
#if defined(REL_WHEEL_HI_RES)
{#enum define RelativeAxis {
    REL_X as RelX,
    REL_Y as RelY,
    REL_Z as RelZ,
    REL_RX as RelRx,
    REL_RY as RelRy,
    REL_RZ as RelRz,
    REL_HWHEEL as RelHwheel,
    REL_DIAL as RelDial,
    REL_WHEEL as RelWheel,
    REL_MISC as RelMisc,
    REL_RESERVED as RelReserved,
    REL_WHEEL_HI_RES as RelWheelHiRes,
    REL_HWHEEL_HI_RES as RelHWheelHiRes}
    deriving (Bounded, Eq, Ord, Read, Show) #}
# else
{#enum define RelativeAxis {
    REL_X as RelX,
    REL_Y as RelY,
    REL_Z as RelZ,
    REL_RX as RelRx,
    REL_RY as RelRy,
    REL_RZ as RelRz,
    REL_HWHEEL as RelHwheel,
    REL_DIAL as RelDial,
    REL_WHEEL as RelWheel,
    REL_MISC as RelMisc,
    REL_RESERVED as RelReserved}
    deriving (Bounded, Eq, Ord, Read, Show) #}
#endif

-- | Absolute changes
{#enum define AbsoluteAxis {
    ABS_X as AbsX,
    ABS_Y as AbsY,
    ABS_Z as AbsZ,
    ABS_RX as AbsRx,
    ABS_RY as AbsRy,
    ABS_RZ as AbsRz,
    ABS_THROTTLE as AbsThrottle,
    ABS_RUDDER as AbsRudder,
    ABS_WHEEL as AbsWheel,
    ABS_GAS as AbsGas,
    ABS_BRAKE as AbsBrake,
    ABS_HAT0X as AbsHat0x,
    ABS_HAT0Y as AbsHat0y,
    ABS_HAT1X as AbsHat1x,
    ABS_HAT1Y as AbsHat1y,
    ABS_HAT2X as AbsHat2x,
    ABS_HAT2Y as AbsHat2y,
    ABS_HAT3X as AbsHat3x,
    ABS_HAT3Y as AbsHat3y,
    ABS_PRESSURE as AbsPressure,
    ABS_DISTANCE as AbsDistance,
    ABS_TILT_X as AbsTiltX,
    ABS_TILT_Y as AbsTiltY,
    ABS_TOOL_WIDTH as AbsToolWidth,
    ABS_VOLUME as AbsVolume,
    ABS_MISC as AbsMisc,
    ABS_RESERVED as AbsReserved,
    ABS_MT_SLOT as AbsMtSlot,
    ABS_MT_TOUCH_MAJOR as AbsMtTouchMajor,
    ABS_MT_TOUCH_MINOR as AbsMtTouchMinor,
    ABS_MT_WIDTH_MAJOR as AbsMtWidthMajor,
    ABS_MT_WIDTH_MINOR as AbsMtWidthMinor,
    ABS_MT_ORIENTATION as AbsMtOrientation,
    ABS_MT_POSITION_X as AbsMtPositionX,
    ABS_MT_POSITION_Y as AbsMtPositionY,
    ABS_MT_TOOL_TYPE as AbsMtToolType,
    ABS_MT_BLOB_ID as AbsMtBlobId,
    ABS_MT_TRACKING_ID as AbsMtTrackingId,
    ABS_MT_PRESSURE as AbsMtPressure,
    ABS_MT_DISTANCE as AbsMtDistance,
    ABS_MT_TOOL_X as AbsMtToolX,
    ABS_MT_TOOL_Y as AbsMtToolY}
    deriving (Bounded, Eq, Ord, Read, Show) #}

-- | Stateful binary switches
{#enum define SwitchEvent {
    SW_LID as SwLid,
    SW_TABLET_MODE as SwTabletMode,
    SW_HEADPHONE_INSERT as SwHeadphoneInsert,
    SW_RFKILL_ALL as SwRfkillAll,
    SW_RADIO as SwRadio,
    SW_MICROPHONE_INSERT as SwMicrophoneInsert,
    SW_DOCK as SwDock,
    SW_LINEOUT_INSERT as SwLineoutInsert,
    SW_JACK_PHYSICAL_INSERT as SwJackPhysicalInsert,
    SW_VIDEOOUT_INSERT as SwVideooutInsert,
    SW_CAMERA_LENS_COVER as SwCameraLensCover,
    SW_KEYPAD_SLIDE as SwKeypadSlide,
    SW_FRONT_PROXIMITY as SwFrontProximity,
    SW_ROTATE_LOCK as SwRotateLock,
    SW_LINEIN_INSERT as SwLineinInsert,
    SW_MUTE_DEVICE as SwMuteDevice}
    deriving (Bounded, Eq, Ord, Read, Show) #}

-- | Miscellaneous
{#enum define MiscEvent {
    MSC_SERIAL as MscSerial,
    MSC_PULSELED as MscPulseled,
    MSC_GESTURE as MscGesture,
    MSC_RAW as MscRaw,
    MSC_SCAN as MscScan,
    MSC_TIMESTAMP as MscTimestamp}
    deriving (Bounded, Eq, Ord, Read, Show) #}

-- | LEDs
{#enum define LEDEvent {
    LED_NUML as LedNuml,
    LED_CAPSL as LedCapsl,
    LED_SCROLLL as LedScrolll,
    LED_COMPOSE as LedCompose,
    LED_KANA as LedKana,
    LED_SLEEP as LedSleep,
    LED_SUSPEND as LedSuspend,
    LED_MUTE as LedMute,
    LED_MISC as LedMisc,
    LED_MAIL as LedMail,
    LED_CHARGING as LedCharging}
    deriving (Bounded, Eq, Ord, Read, Show) #}

-- | Specifying autorepeating events
{#enum define RepeatEvent {
    REP_DELAY as RepDelay,
    REP_PERIOD as RepPeriod}
    deriving (Bounded, Eq, Ord, Read, Show) #}

-- | For simple sound output devices
{#enum define SoundEvent {
    SND_CLICK as SndClick,
    SND_BELL as SndBell,
    SND_TONE as SndTone}
    deriving (Bounded, Eq, Ord, Read, Show) #}

-- | Device properties
{#enum define DeviceProperty {
    INPUT_PROP_POINTER as InputPropPointer,
    INPUT_PROP_DIRECT as InputPropDirect,
    INPUT_PROP_BUTTONPAD as InputPropButtonpad,
    INPUT_PROP_SEMI_MT as InputPropSemiMt,
    INPUT_PROP_TOPBUTTONPAD as InputPropTopbuttonpad,
    INPUT_PROP_POINTING_STICK as InputPropPointingStick,
    INPUT_PROP_ACCELEROMETER as InputPropAccelerometer}
    deriving (Bounded, Eq, Ord, Read, Show) #}
