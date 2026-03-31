{-# LANGUAGE PatternSynonyms #-}

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





-- | Each of these corresponds to one of the contructors of 'Evdev.EventData'. So you're unlikely to need to use these directly (C doesn't have ADTs - we do).
data EventType = EvSyn
               | EvKey
               | EvRel
               | EvAbs
               | EvMsc
               | EvSw
               | EvLed
               | EvSnd
               | EvRep
               | EvFf
               | EvPwr
               | EvFfStatus
  deriving (Bounded,Eq,Ord,Read,Show)
instance Enum EventType where
  succ EvSyn = EvKey
  succ EvKey = EvRel
  succ EvRel = EvAbs
  succ EvAbs = EvMsc
  succ EvMsc = EvSw
  succ EvSw = EvLed
  succ EvLed = EvSnd
  succ EvSnd = EvRep
  succ EvRep = EvFf
  succ EvFf = EvPwr
  succ EvPwr = EvFfStatus
  succ EvFfStatus = error "EventType.succ: EvFfStatus has no successor"

  pred EvKey = EvSyn
  pred EvRel = EvKey
  pred EvAbs = EvRel
  pred EvMsc = EvAbs
  pred EvSw = EvMsc
  pred EvLed = EvSw
  pred EvSnd = EvLed
  pred EvRep = EvSnd
  pred EvFf = EvRep
  pred EvPwr = EvFf
  pred EvFfStatus = EvPwr
  pred EvSyn = error "EventType.pred: EvSyn has no predecessor"

  enumFromTo from to = go from
    where
      end = fromEnum to
      go v = case compare (fromEnum v) end of
                 LT -> v : go (succ v)
                 EQ -> [v]
                 GT -> []

  enumFrom from = enumFromTo from EvFfStatus

  fromEnum EvSyn = 0
  fromEnum EvKey = 1
  fromEnum EvRel = 2
  fromEnum EvAbs = 3
  fromEnum EvMsc = 4
  fromEnum EvSw = 5
  fromEnum EvLed = 17
  fromEnum EvSnd = 18
  fromEnum EvRep = 20
  fromEnum EvFf = 21
  fromEnum EvPwr = 22
  fromEnum EvFfStatus = 23

  toEnum 0 = EvSyn
  toEnum 1 = EvKey
  toEnum 2 = EvRel
  toEnum 3 = EvAbs
  toEnum 4 = EvMsc
  toEnum 5 = EvSw
  toEnum 17 = EvLed
  toEnum 18 = EvSnd
  toEnum 20 = EvRep
  toEnum 21 = EvFf
  toEnum 22 = EvPwr
  toEnum 23 = EvFfStatus
  toEnum unmatched = error ("EventType.toEnum: Cannot match " ++ show unmatched)



-- | Synchronization events
data SyncEvent = SynReport
               | SynConfig
               | SynMtReport
               | SynDropped
  deriving (Bounded,Eq,Ord,Read,Show)
instance Enum SyncEvent where
  succ SynReport = SynConfig
  succ SynConfig = SynMtReport
  succ SynMtReport = SynDropped
  succ SynDropped = error "SyncEvent.succ: SynDropped has no successor"

  pred SynConfig = SynReport
  pred SynMtReport = SynConfig
  pred SynDropped = SynMtReport
  pred SynReport = error "SyncEvent.pred: SynReport has no predecessor"

  enumFromTo from to = go from
    where
      end = fromEnum to
      go v = case compare (fromEnum v) end of
                 LT -> v : go (succ v)
                 EQ -> [v]
                 GT -> []

  enumFrom from = enumFromTo from SynDropped

  fromEnum SynReport = 0
  fromEnum SynConfig = 1
  fromEnum SynMtReport = 2
  fromEnum SynDropped = 3

  toEnum 0 = SynReport
  toEnum 1 = SynConfig
  toEnum 2 = SynMtReport
  toEnum 3 = SynDropped
  toEnum unmatched = error ("SyncEvent.toEnum: Cannot match " ++ show unmatched)



-- | Keys and buttons
data Key = KeyReserved
         | KeyEsc
         | Key1
         | Key2
         | Key3
         | Key4
         | Key5
         | Key6
         | Key7
         | Key8
         | Key9
         | Key0
         | KeyMinus
         | KeyEqual
         | KeyBackspace
         | KeyTab
         | KeyQ
         | KeyW
         | KeyE
         | KeyR
         | KeyT
         | KeyY
         | KeyU
         | KeyI
         | KeyO
         | KeyP
         | KeyLeftbrace
         | KeyRightbrace
         | KeyEnter
         | KeyLeftctrl
         | KeyA
         | KeyS
         | KeyD
         | KeyF
         | KeyG
         | KeyH
         | KeyJ
         | KeyK
         | KeyL
         | KeySemicolon
         | KeyApostrophe
         | KeyGrave
         | KeyLeftshift
         | KeyBackslash
         | KeyZ
         | KeyX
         | KeyC
         | KeyV
         | KeyB
         | KeyN
         | KeyM
         | KeyComma
         | KeyDot
         | KeySlash
         | KeyRightshift
         | KeyKpasterisk
         | KeyLeftalt
         | KeySpace
         | KeyCapslock
         | KeyF1
         | KeyF2
         | KeyF3
         | KeyF4
         | KeyF5
         | KeyF6
         | KeyF7
         | KeyF8
         | KeyF9
         | KeyF10
         | KeyNumlock
         | KeyScrolllock
         | KeyKp7
         | KeyKp8
         | KeyKp9
         | KeyKpminus
         | KeyKp4
         | KeyKp5
         | KeyKp6
         | KeyKpplus
         | KeyKp1
         | KeyKp2
         | KeyKp3
         | KeyKp0
         | KeyKpdot
         | KeyZenkakuhankaku
         | Key102nd
         | KeyF11
         | KeyF12
         | KeyRo
         | KeyKatakana
         | KeyHiragana
         | KeyHenkan
         | KeyKatakanahiragana
         | KeyMuhenkan
         | KeyKpjpcomma
         | KeyKpenter
         | KeyRightctrl
         | KeyKpslash
         | KeySysrq
         | KeyRightalt
         | KeyLinefeed
         | KeyHome
         | KeyUp
         | KeyPageup
         | KeyLeft
         | KeyRight
         | KeyEnd
         | KeyDown
         | KeyPagedown
         | KeyInsert
         | KeyDelete
         | KeyMacro
         | KeyMute
         | KeyVolumedown
         | KeyVolumeup
         | KeyPower
         | KeyKpequal
         | KeyKpplusminus
         | KeyPause
         | KeyScale
         | KeyKpcomma
         | KeyHangeul
         | KeyHanja
         | KeyYen
         | KeyLeftmeta
         | KeyRightmeta
         | KeyCompose
         | KeyStop
         | KeyAgain
         | KeyProps
         | KeyUndo
         | KeyFront
         | KeyCopy
         | KeyOpen
         | KeyPaste
         | KeyFind
         | KeyCut
         | KeyHelp
         | KeyMenu
         | KeyCalc
         | KeySetup
         | KeySleep
         | KeyWakeup
         | KeyFile
         | KeySendfile
         | KeyDeletefile
         | KeyXfer
         | KeyProg1
         | KeyProg2
         | KeyWww
         | KeyMsdos
         | KeyScreenlock
         | KeyRotateDisplay
         | KeyCyclewindows
         | KeyMail
         | KeyBookmarks
         | KeyComputer
         | KeyBack
         | KeyForward
         | KeyClosecd
         | KeyEjectcd
         | KeyEjectclosecd
         | KeyNextsong
         | KeyPlaypause
         | KeyPrevioussong
         | KeyStopcd
         | KeyRecord
         | KeyRewind
         | KeyPhone
         | KeyIso
         | KeyConfig
         | KeyHomepage
         | KeyRefresh
         | KeyExit
         | KeyMove
         | KeyEdit
         | KeyScrollup
         | KeyScrolldown
         | KeyKpleftparen
         | KeyKprightparen
         | KeyNew
         | KeyRedo
         | KeyF13
         | KeyF14
         | KeyF15
         | KeyF16
         | KeyF17
         | KeyF18
         | KeyF19
         | KeyF20
         | KeyF21
         | KeyF22
         | KeyF23
         | KeyF24
         | KeyPlaycd
         | KeyPausecd
         | KeyProg3
         | KeyProg4
         | KeyDashboard
         | KeySuspend
         | KeyClose
         | KeyPlay
         | KeyFastforward
         | KeyBassboost
         | KeyPrint
         | KeyHp
         | KeyCamera
         | KeySound
         | KeyQuestion
         | KeyEmail
         | KeyChat
         | KeySearch
         | KeyConnect
         | KeyFinance
         | KeySport
         | KeyShop
         | KeyAlterase
         | KeyCancel
         | KeyBrightnessdown
         | KeyBrightnessup
         | KeyMedia
         | KeySwitchvideomode
         | KeyKbdillumtoggle
         | KeyKbdillumdown
         | KeyKbdillumup
         | KeySend
         | KeyReply
         | KeyForwardmail
         | KeySave
         | KeyDocuments
         | KeyBattery
         | KeyBluetooth
         | KeyWlan
         | KeyUwb
         | KeyUnknown
         | KeyVideoNext
         | KeyVideoPrev
         | KeyBrightnessCycle
         | KeyBrightnessAuto
         | KeyDisplayOff
         | KeyWwan
         | KeyRfkill
         | KeyMicmute
         | Btn0
         | Btn1
         | Btn2
         | Btn3
         | Btn4
         | Btn5
         | Btn6
         | Btn7
         | Btn8
         | Btn9
         | BtnLeft
         | BtnRight
         | BtnMiddle
         | BtnSide
         | BtnExtra
         | BtnForward
         | BtnBack
         | BtnTask
         | BtnJoystick
         | BtnThumb
         | BtnThumb2
         | BtnTop
         | BtnTop2
         | BtnPinkie
         | BtnBase
         | BtnBase2
         | BtnBase3
         | BtnBase4
         | BtnBase5
         | BtnBase6
         | BtnDead
         | BtnA
         | BtnB
         | BtnC
         | BtnX
         | BtnY
         | BtnZ
         | BtnTl
         | BtnTr
         | BtnTl2
         | BtnTr2
         | BtnSelect
         | BtnStart
         | BtnMode
         | BtnThumbl
         | BtnThumbr
         | BtnToolPen
         | BtnToolRubber
         | BtnToolBrush
         | BtnToolPencil
         | BtnToolAirbrush
         | BtnToolFinger
         | BtnToolMouse
         | BtnToolLens
         | BtnToolQuinttap
         | BtnTouch
         | BtnStylus
         | BtnStylus2
         | BtnToolDoubletap
         | BtnToolTripletap
         | BtnToolQuadtap
         | BtnGearDown
         | BtnGearUp
         | KeyOk
         | KeySelect
         | KeyGoto
         | KeyClear
         | KeyPower2
         | KeyOption
         | KeyInfo
         | KeyTime
         | KeyVendor
         | KeyArchive
         | KeyProgram
         | KeyChannel
         | KeyFavorites
         | KeyEpg
         | KeyPvr
         | KeyMhp
         | KeyLanguage
         | KeyTitle
         | KeySubtitle
         | KeyAngle
         | KeyZoom
         | KeyMode
         | KeyKeyboard
         | KeyScreen
         | KeyPc
         | KeyTv
         | KeyTv2
         | KeyVcr
         | KeyVcr2
         | KeySat
         | KeySat2
         | KeyCd
         | KeyTape
         | KeyRadio
         | KeyTuner
         | KeyPlayer
         | KeyText
         | KeyDvd
         | KeyAux
         | KeyMp3
         | KeyAudio
         | KeyVideo
         | KeyDirectory
         | KeyList
         | KeyMemo
         | KeyCalendar
         | KeyRed
         | KeyGreen
         | KeyYellow
         | KeyBlue
         | KeyChannelup
         | KeyChanneldown
         | KeyFirst
         | KeyLast
         | KeyAb
         | KeyNext
         | KeyRestart
         | KeySlow
         | KeyShuffle
         | KeyBreak
         | KeyPrevious
         | KeyDigits
         | KeyTeen
         | KeyTwen
         | KeyVideophone
         | KeyGames
         | KeyZoomin
         | KeyZoomout
         | KeyZoomreset
         | KeyWordprocessor
         | KeyEditor
         | KeySpreadsheet
         | KeyGraphicseditor
         | KeyPresentation
         | KeyDatabase
         | KeyNews
         | KeyVoicemail
         | KeyAddressbook
         | KeyMessenger
         | KeyDisplaytoggle
         | KeySpellcheck
         | KeyLogoff
         | KeyDollar
         | KeyEuro
         | KeyFrameback
         | KeyFrameforward
         | KeyContextMenu
         | KeyMediaRepeat
         | Key10channelsup
         | Key10channelsdown
         | KeyImages
         | KeyDelEol
         | KeyDelEos
         | KeyInsLine
         | KeyDelLine
         | KeyFn
         | KeyFnEsc
         | KeyFnF1
         | KeyFnF2
         | KeyFnF3
         | KeyFnF4
         | KeyFnF5
         | KeyFnF6
         | KeyFnF7
         | KeyFnF8
         | KeyFnF9
         | KeyFnF10
         | KeyFnF11
         | KeyFnF12
         | KeyFn1
         | KeyFn2
         | KeyFnD
         | KeyFnE
         | KeyFnF
         | KeyFnS
         | KeyFnB
         | KeyBrlDot1
         | KeyBrlDot2
         | KeyBrlDot3
         | KeyBrlDot4
         | KeyBrlDot5
         | KeyBrlDot6
         | KeyBrlDot7
         | KeyBrlDot8
         | KeyBrlDot9
         | KeyBrlDot10
         | KeyNumeric0
         | KeyNumeric1
         | KeyNumeric2
         | KeyNumeric3
         | KeyNumeric4
         | KeyNumeric5
         | KeyNumeric6
         | KeyNumeric7
         | KeyNumeric8
         | KeyNumeric9
         | KeyNumericStar
         | KeyNumericPound
         | KeyNumericA
         | KeyNumericB
         | KeyNumericC
         | KeyNumericD
         | KeyCameraFocus
         | KeyWpsButton
         | KeyTouchpadToggle
         | KeyTouchpadOn
         | KeyTouchpadOff
         | KeyCameraZoomin
         | KeyCameraZoomout
         | KeyCameraUp
         | KeyCameraDown
         | KeyCameraLeft
         | KeyCameraRight
         | KeyAttendantOn
         | KeyAttendantOff
         | KeyAttendantToggle
         | KeyLightsToggle
         | BtnDpadUp
         | BtnDpadDown
         | BtnDpadLeft
         | BtnDpadRight
         | KeyAlsToggle
         | KeyButtonconfig
         | KeyTaskmanager
         | KeyJournal
         | KeyControlpanel
         | KeyAppselect
         | KeyScreensaver
         | KeyVoicecommand
         | KeyBrightnessMin
         | KeyBrightnessMax
         | KeyKbdinputassistPrev
         | KeyKbdinputassistNext
         | KeyKbdinputassistPrevgroup
         | KeyKbdinputassistNextgroup
         | KeyKbdinputassistAccept
         | KeyKbdinputassistCancel
         | BtnTriggerHappy1
         | BtnTriggerHappy2
         | BtnTriggerHappy3
         | BtnTriggerHappy4
         | BtnTriggerHappy5
         | BtnTriggerHappy6
         | BtnTriggerHappy7
         | BtnTriggerHappy8
         | BtnTriggerHappy9
         | BtnTriggerHappy10
         | BtnTriggerHappy11
         | BtnTriggerHappy12
         | BtnTriggerHappy13
         | BtnTriggerHappy14
         | BtnTriggerHappy15
         | BtnTriggerHappy16
         | BtnTriggerHappy17
         | BtnTriggerHappy18
         | BtnTriggerHappy19
         | BtnTriggerHappy20
         | BtnTriggerHappy21
         | BtnTriggerHappy22
         | BtnTriggerHappy23
         | BtnTriggerHappy24
         | BtnTriggerHappy25
         | BtnTriggerHappy26
         | BtnTriggerHappy27
         | BtnTriggerHappy28
         | BtnTriggerHappy29
         | BtnTriggerHappy30
         | BtnTriggerHappy31
         | BtnTriggerHappy32
         | BtnTriggerHappy33
         | BtnTriggerHappy34
         | BtnTriggerHappy35
         | BtnTriggerHappy36
         | BtnTriggerHappy37
         | BtnTriggerHappy38
         | BtnTriggerHappy39
         | BtnTriggerHappy40
  deriving (Bounded,Eq,Ord,Read,Show)
instance Enum Key where
  succ KeyReserved = KeyEsc
  succ KeyEsc = Key1
  succ Key1 = Key2
  succ Key2 = Key3
  succ Key3 = Key4
  succ Key4 = Key5
  succ Key5 = Key6
  succ Key6 = Key7
  succ Key7 = Key8
  succ Key8 = Key9
  succ Key9 = Key0
  succ Key0 = KeyMinus
  succ KeyMinus = KeyEqual
  succ KeyEqual = KeyBackspace
  succ KeyBackspace = KeyTab
  succ KeyTab = KeyQ
  succ KeyQ = KeyW
  succ KeyW = KeyE
  succ KeyE = KeyR
  succ KeyR = KeyT
  succ KeyT = KeyY
  succ KeyY = KeyU
  succ KeyU = KeyI
  succ KeyI = KeyO
  succ KeyO = KeyP
  succ KeyP = KeyLeftbrace
  succ KeyLeftbrace = KeyRightbrace
  succ KeyRightbrace = KeyEnter
  succ KeyEnter = KeyLeftctrl
  succ KeyLeftctrl = KeyA
  succ KeyA = KeyS
  succ KeyS = KeyD
  succ KeyD = KeyF
  succ KeyF = KeyG
  succ KeyG = KeyH
  succ KeyH = KeyJ
  succ KeyJ = KeyK
  succ KeyK = KeyL
  succ KeyL = KeySemicolon
  succ KeySemicolon = KeyApostrophe
  succ KeyApostrophe = KeyGrave
  succ KeyGrave = KeyLeftshift
  succ KeyLeftshift = KeyBackslash
  succ KeyBackslash = KeyZ
  succ KeyZ = KeyX
  succ KeyX = KeyC
  succ KeyC = KeyV
  succ KeyV = KeyB
  succ KeyB = KeyN
  succ KeyN = KeyM
  succ KeyM = KeyComma
  succ KeyComma = KeyDot
  succ KeyDot = KeySlash
  succ KeySlash = KeyRightshift
  succ KeyRightshift = KeyKpasterisk
  succ KeyKpasterisk = KeyLeftalt
  succ KeyLeftalt = KeySpace
  succ KeySpace = KeyCapslock
  succ KeyCapslock = KeyF1
  succ KeyF1 = KeyF2
  succ KeyF2 = KeyF3
  succ KeyF3 = KeyF4
  succ KeyF4 = KeyF5
  succ KeyF5 = KeyF6
  succ KeyF6 = KeyF7
  succ KeyF7 = KeyF8
  succ KeyF8 = KeyF9
  succ KeyF9 = KeyF10
  succ KeyF10 = KeyNumlock
  succ KeyNumlock = KeyScrolllock
  succ KeyScrolllock = KeyKp7
  succ KeyKp7 = KeyKp8
  succ KeyKp8 = KeyKp9
  succ KeyKp9 = KeyKpminus
  succ KeyKpminus = KeyKp4
  succ KeyKp4 = KeyKp5
  succ KeyKp5 = KeyKp6
  succ KeyKp6 = KeyKpplus
  succ KeyKpplus = KeyKp1
  succ KeyKp1 = KeyKp2
  succ KeyKp2 = KeyKp3
  succ KeyKp3 = KeyKp0
  succ KeyKp0 = KeyKpdot
  succ KeyKpdot = KeyZenkakuhankaku
  succ KeyZenkakuhankaku = Key102nd
  succ Key102nd = KeyF11
  succ KeyF11 = KeyF12
  succ KeyF12 = KeyRo
  succ KeyRo = KeyKatakana
  succ KeyKatakana = KeyHiragana
  succ KeyHiragana = KeyHenkan
  succ KeyHenkan = KeyKatakanahiragana
  succ KeyKatakanahiragana = KeyMuhenkan
  succ KeyMuhenkan = KeyKpjpcomma
  succ KeyKpjpcomma = KeyKpenter
  succ KeyKpenter = KeyRightctrl
  succ KeyRightctrl = KeyKpslash
  succ KeyKpslash = KeySysrq
  succ KeySysrq = KeyRightalt
  succ KeyRightalt = KeyLinefeed
  succ KeyLinefeed = KeyHome
  succ KeyHome = KeyUp
  succ KeyUp = KeyPageup
  succ KeyPageup = KeyLeft
  succ KeyLeft = KeyRight
  succ KeyRight = KeyEnd
  succ KeyEnd = KeyDown
  succ KeyDown = KeyPagedown
  succ KeyPagedown = KeyInsert
  succ KeyInsert = KeyDelete
  succ KeyDelete = KeyMacro
  succ KeyMacro = KeyMute
  succ KeyMute = KeyVolumedown
  succ KeyVolumedown = KeyVolumeup
  succ KeyVolumeup = KeyPower
  succ KeyPower = KeyKpequal
  succ KeyKpequal = KeyKpplusminus
  succ KeyKpplusminus = KeyPause
  succ KeyPause = KeyScale
  succ KeyScale = KeyKpcomma
  succ KeyKpcomma = KeyHangeul
  succ KeyHangeul = KeyHanja
  succ KeyHanja = KeyYen
  succ KeyYen = KeyLeftmeta
  succ KeyLeftmeta = KeyRightmeta
  succ KeyRightmeta = KeyCompose
  succ KeyCompose = KeyStop
  succ KeyStop = KeyAgain
  succ KeyAgain = KeyProps
  succ KeyProps = KeyUndo
  succ KeyUndo = KeyFront
  succ KeyFront = KeyCopy
  succ KeyCopy = KeyOpen
  succ KeyOpen = KeyPaste
  succ KeyPaste = KeyFind
  succ KeyFind = KeyCut
  succ KeyCut = KeyHelp
  succ KeyHelp = KeyMenu
  succ KeyMenu = KeyCalc
  succ KeyCalc = KeySetup
  succ KeySetup = KeySleep
  succ KeySleep = KeyWakeup
  succ KeyWakeup = KeyFile
  succ KeyFile = KeySendfile
  succ KeySendfile = KeyDeletefile
  succ KeyDeletefile = KeyXfer
  succ KeyXfer = KeyProg1
  succ KeyProg1 = KeyProg2
  succ KeyProg2 = KeyWww
  succ KeyWww = KeyMsdos
  succ KeyMsdos = KeyScreenlock
  succ KeyScreenlock = KeyRotateDisplay
  succ KeyRotateDisplay = KeyCyclewindows
  succ KeyCyclewindows = KeyMail
  succ KeyMail = KeyBookmarks
  succ KeyBookmarks = KeyComputer
  succ KeyComputer = KeyBack
  succ KeyBack = KeyForward
  succ KeyForward = KeyClosecd
  succ KeyClosecd = KeyEjectcd
  succ KeyEjectcd = KeyEjectclosecd
  succ KeyEjectclosecd = KeyNextsong
  succ KeyNextsong = KeyPlaypause
  succ KeyPlaypause = KeyPrevioussong
  succ KeyPrevioussong = KeyStopcd
  succ KeyStopcd = KeyRecord
  succ KeyRecord = KeyRewind
  succ KeyRewind = KeyPhone
  succ KeyPhone = KeyIso
  succ KeyIso = KeyConfig
  succ KeyConfig = KeyHomepage
  succ KeyHomepage = KeyRefresh
  succ KeyRefresh = KeyExit
  succ KeyExit = KeyMove
  succ KeyMove = KeyEdit
  succ KeyEdit = KeyScrollup
  succ KeyScrollup = KeyScrolldown
  succ KeyScrolldown = KeyKpleftparen
  succ KeyKpleftparen = KeyKprightparen
  succ KeyKprightparen = KeyNew
  succ KeyNew = KeyRedo
  succ KeyRedo = KeyF13
  succ KeyF13 = KeyF14
  succ KeyF14 = KeyF15
  succ KeyF15 = KeyF16
  succ KeyF16 = KeyF17
  succ KeyF17 = KeyF18
  succ KeyF18 = KeyF19
  succ KeyF19 = KeyF20
  succ KeyF20 = KeyF21
  succ KeyF21 = KeyF22
  succ KeyF22 = KeyF23
  succ KeyF23 = KeyF24
  succ KeyF24 = KeyPlaycd
  succ KeyPlaycd = KeyPausecd
  succ KeyPausecd = KeyProg3
  succ KeyProg3 = KeyProg4
  succ KeyProg4 = KeyDashboard
  succ KeyDashboard = KeySuspend
  succ KeySuspend = KeyClose
  succ KeyClose = KeyPlay
  succ KeyPlay = KeyFastforward
  succ KeyFastforward = KeyBassboost
  succ KeyBassboost = KeyPrint
  succ KeyPrint = KeyHp
  succ KeyHp = KeyCamera
  succ KeyCamera = KeySound
  succ KeySound = KeyQuestion
  succ KeyQuestion = KeyEmail
  succ KeyEmail = KeyChat
  succ KeyChat = KeySearch
  succ KeySearch = KeyConnect
  succ KeyConnect = KeyFinance
  succ KeyFinance = KeySport
  succ KeySport = KeyShop
  succ KeyShop = KeyAlterase
  succ KeyAlterase = KeyCancel
  succ KeyCancel = KeyBrightnessdown
  succ KeyBrightnessdown = KeyBrightnessup
  succ KeyBrightnessup = KeyMedia
  succ KeyMedia = KeySwitchvideomode
  succ KeySwitchvideomode = KeyKbdillumtoggle
  succ KeyKbdillumtoggle = KeyKbdillumdown
  succ KeyKbdillumdown = KeyKbdillumup
  succ KeyKbdillumup = KeySend
  succ KeySend = KeyReply
  succ KeyReply = KeyForwardmail
  succ KeyForwardmail = KeySave
  succ KeySave = KeyDocuments
  succ KeyDocuments = KeyBattery
  succ KeyBattery = KeyBluetooth
  succ KeyBluetooth = KeyWlan
  succ KeyWlan = KeyUwb
  succ KeyUwb = KeyUnknown
  succ KeyUnknown = KeyVideoNext
  succ KeyVideoNext = KeyVideoPrev
  succ KeyVideoPrev = KeyBrightnessCycle
  succ KeyBrightnessCycle = KeyBrightnessAuto
  succ KeyBrightnessAuto = KeyDisplayOff
  succ KeyDisplayOff = KeyWwan
  succ KeyWwan = KeyRfkill
  succ KeyRfkill = KeyMicmute
  succ KeyMicmute = Btn0
  succ Btn0 = Btn1
  succ Btn1 = Btn2
  succ Btn2 = Btn3
  succ Btn3 = Btn4
  succ Btn4 = Btn5
  succ Btn5 = Btn6
  succ Btn6 = Btn7
  succ Btn7 = Btn8
  succ Btn8 = Btn9
  succ Btn9 = BtnLeft
  succ BtnLeft = BtnRight
  succ BtnRight = BtnMiddle
  succ BtnMiddle = BtnSide
  succ BtnSide = BtnExtra
  succ BtnExtra = BtnForward
  succ BtnForward = BtnBack
  succ BtnBack = BtnTask
  succ BtnTask = BtnJoystick
  succ BtnJoystick = BtnThumb
  succ BtnThumb = BtnThumb2
  succ BtnThumb2 = BtnTop
  succ BtnTop = BtnTop2
  succ BtnTop2 = BtnPinkie
  succ BtnPinkie = BtnBase
  succ BtnBase = BtnBase2
  succ BtnBase2 = BtnBase3
  succ BtnBase3 = BtnBase4
  succ BtnBase4 = BtnBase5
  succ BtnBase5 = BtnBase6
  succ BtnBase6 = BtnDead
  succ BtnDead = BtnA
  succ BtnA = BtnB
  succ BtnB = BtnC
  succ BtnC = BtnX
  succ BtnX = BtnY
  succ BtnY = BtnZ
  succ BtnZ = BtnTl
  succ BtnTl = BtnTr
  succ BtnTr = BtnTl2
  succ BtnTl2 = BtnTr2
  succ BtnTr2 = BtnSelect
  succ BtnSelect = BtnStart
  succ BtnStart = BtnMode
  succ BtnMode = BtnThumbl
  succ BtnThumbl = BtnThumbr
  succ BtnThumbr = BtnToolPen
  succ BtnToolPen = BtnToolRubber
  succ BtnToolRubber = BtnToolBrush
  succ BtnToolBrush = BtnToolPencil
  succ BtnToolPencil = BtnToolAirbrush
  succ BtnToolAirbrush = BtnToolFinger
  succ BtnToolFinger = BtnToolMouse
  succ BtnToolMouse = BtnToolLens
  succ BtnToolLens = BtnToolQuinttap
  succ BtnToolQuinttap = BtnTouch
  succ BtnTouch = BtnStylus
  succ BtnStylus = BtnStylus2
  succ BtnStylus2 = BtnToolDoubletap
  succ BtnToolDoubletap = BtnToolTripletap
  succ BtnToolTripletap = BtnToolQuadtap
  succ BtnToolQuadtap = BtnGearDown
  succ BtnGearDown = BtnGearUp
  succ BtnGearUp = KeyOk
  succ KeyOk = KeySelect
  succ KeySelect = KeyGoto
  succ KeyGoto = KeyClear
  succ KeyClear = KeyPower2
  succ KeyPower2 = KeyOption
  succ KeyOption = KeyInfo
  succ KeyInfo = KeyTime
  succ KeyTime = KeyVendor
  succ KeyVendor = KeyArchive
  succ KeyArchive = KeyProgram
  succ KeyProgram = KeyChannel
  succ KeyChannel = KeyFavorites
  succ KeyFavorites = KeyEpg
  succ KeyEpg = KeyPvr
  succ KeyPvr = KeyMhp
  succ KeyMhp = KeyLanguage
  succ KeyLanguage = KeyTitle
  succ KeyTitle = KeySubtitle
  succ KeySubtitle = KeyAngle
  succ KeyAngle = KeyZoom
  succ KeyZoom = KeyMode
  succ KeyMode = KeyKeyboard
  succ KeyKeyboard = KeyScreen
  succ KeyScreen = KeyPc
  succ KeyPc = KeyTv
  succ KeyTv = KeyTv2
  succ KeyTv2 = KeyVcr
  succ KeyVcr = KeyVcr2
  succ KeyVcr2 = KeySat
  succ KeySat = KeySat2
  succ KeySat2 = KeyCd
  succ KeyCd = KeyTape
  succ KeyTape = KeyRadio
  succ KeyRadio = KeyTuner
  succ KeyTuner = KeyPlayer
  succ KeyPlayer = KeyText
  succ KeyText = KeyDvd
  succ KeyDvd = KeyAux
  succ KeyAux = KeyMp3
  succ KeyMp3 = KeyAudio
  succ KeyAudio = KeyVideo
  succ KeyVideo = KeyDirectory
  succ KeyDirectory = KeyList
  succ KeyList = KeyMemo
  succ KeyMemo = KeyCalendar
  succ KeyCalendar = KeyRed
  succ KeyRed = KeyGreen
  succ KeyGreen = KeyYellow
  succ KeyYellow = KeyBlue
  succ KeyBlue = KeyChannelup
  succ KeyChannelup = KeyChanneldown
  succ KeyChanneldown = KeyFirst
  succ KeyFirst = KeyLast
  succ KeyLast = KeyAb
  succ KeyAb = KeyNext
  succ KeyNext = KeyRestart
  succ KeyRestart = KeySlow
  succ KeySlow = KeyShuffle
  succ KeyShuffle = KeyBreak
  succ KeyBreak = KeyPrevious
  succ KeyPrevious = KeyDigits
  succ KeyDigits = KeyTeen
  succ KeyTeen = KeyTwen
  succ KeyTwen = KeyVideophone
  succ KeyVideophone = KeyGames
  succ KeyGames = KeyZoomin
  succ KeyZoomin = KeyZoomout
  succ KeyZoomout = KeyZoomreset
  succ KeyZoomreset = KeyWordprocessor
  succ KeyWordprocessor = KeyEditor
  succ KeyEditor = KeySpreadsheet
  succ KeySpreadsheet = KeyGraphicseditor
  succ KeyGraphicseditor = KeyPresentation
  succ KeyPresentation = KeyDatabase
  succ KeyDatabase = KeyNews
  succ KeyNews = KeyVoicemail
  succ KeyVoicemail = KeyAddressbook
  succ KeyAddressbook = KeyMessenger
  succ KeyMessenger = KeyDisplaytoggle
  succ KeyDisplaytoggle = KeySpellcheck
  succ KeySpellcheck = KeyLogoff
  succ KeyLogoff = KeyDollar
  succ KeyDollar = KeyEuro
  succ KeyEuro = KeyFrameback
  succ KeyFrameback = KeyFrameforward
  succ KeyFrameforward = KeyContextMenu
  succ KeyContextMenu = KeyMediaRepeat
  succ KeyMediaRepeat = Key10channelsup
  succ Key10channelsup = Key10channelsdown
  succ Key10channelsdown = KeyImages
  succ KeyImages = KeyDelEol
  succ KeyDelEol = KeyDelEos
  succ KeyDelEos = KeyInsLine
  succ KeyInsLine = KeyDelLine
  succ KeyDelLine = KeyFn
  succ KeyFn = KeyFnEsc
  succ KeyFnEsc = KeyFnF1
  succ KeyFnF1 = KeyFnF2
  succ KeyFnF2 = KeyFnF3
  succ KeyFnF3 = KeyFnF4
  succ KeyFnF4 = KeyFnF5
  succ KeyFnF5 = KeyFnF6
  succ KeyFnF6 = KeyFnF7
  succ KeyFnF7 = KeyFnF8
  succ KeyFnF8 = KeyFnF9
  succ KeyFnF9 = KeyFnF10
  succ KeyFnF10 = KeyFnF11
  succ KeyFnF11 = KeyFnF12
  succ KeyFnF12 = KeyFn1
  succ KeyFn1 = KeyFn2
  succ KeyFn2 = KeyFnD
  succ KeyFnD = KeyFnE
  succ KeyFnE = KeyFnF
  succ KeyFnF = KeyFnS
  succ KeyFnS = KeyFnB
  succ KeyFnB = KeyBrlDot1
  succ KeyBrlDot1 = KeyBrlDot2
  succ KeyBrlDot2 = KeyBrlDot3
  succ KeyBrlDot3 = KeyBrlDot4
  succ KeyBrlDot4 = KeyBrlDot5
  succ KeyBrlDot5 = KeyBrlDot6
  succ KeyBrlDot6 = KeyBrlDot7
  succ KeyBrlDot7 = KeyBrlDot8
  succ KeyBrlDot8 = KeyBrlDot9
  succ KeyBrlDot9 = KeyBrlDot10
  succ KeyBrlDot10 = KeyNumeric0
  succ KeyNumeric0 = KeyNumeric1
  succ KeyNumeric1 = KeyNumeric2
  succ KeyNumeric2 = KeyNumeric3
  succ KeyNumeric3 = KeyNumeric4
  succ KeyNumeric4 = KeyNumeric5
  succ KeyNumeric5 = KeyNumeric6
  succ KeyNumeric6 = KeyNumeric7
  succ KeyNumeric7 = KeyNumeric8
  succ KeyNumeric8 = KeyNumeric9
  succ KeyNumeric9 = KeyNumericStar
  succ KeyNumericStar = KeyNumericPound
  succ KeyNumericPound = KeyNumericA
  succ KeyNumericA = KeyNumericB
  succ KeyNumericB = KeyNumericC
  succ KeyNumericC = KeyNumericD
  succ KeyNumericD = KeyCameraFocus
  succ KeyCameraFocus = KeyWpsButton
  succ KeyWpsButton = KeyTouchpadToggle
  succ KeyTouchpadToggle = KeyTouchpadOn
  succ KeyTouchpadOn = KeyTouchpadOff
  succ KeyTouchpadOff = KeyCameraZoomin
  succ KeyCameraZoomin = KeyCameraZoomout
  succ KeyCameraZoomout = KeyCameraUp
  succ KeyCameraUp = KeyCameraDown
  succ KeyCameraDown = KeyCameraLeft
  succ KeyCameraLeft = KeyCameraRight
  succ KeyCameraRight = KeyAttendantOn
  succ KeyAttendantOn = KeyAttendantOff
  succ KeyAttendantOff = KeyAttendantToggle
  succ KeyAttendantToggle = KeyLightsToggle
  succ KeyLightsToggle = BtnDpadUp
  succ BtnDpadUp = BtnDpadDown
  succ BtnDpadDown = BtnDpadLeft
  succ BtnDpadLeft = BtnDpadRight
  succ BtnDpadRight = KeyAlsToggle
  succ KeyAlsToggle = KeyButtonconfig
  succ KeyButtonconfig = KeyTaskmanager
  succ KeyTaskmanager = KeyJournal
  succ KeyJournal = KeyControlpanel
  succ KeyControlpanel = KeyAppselect
  succ KeyAppselect = KeyScreensaver
  succ KeyScreensaver = KeyVoicecommand
  succ KeyVoicecommand = KeyBrightnessMin
  succ KeyBrightnessMin = KeyBrightnessMax
  succ KeyBrightnessMax = KeyKbdinputassistPrev
  succ KeyKbdinputassistPrev = KeyKbdinputassistNext
  succ KeyKbdinputassistNext = KeyKbdinputassistPrevgroup
  succ KeyKbdinputassistPrevgroup = KeyKbdinputassistNextgroup
  succ KeyKbdinputassistNextgroup = KeyKbdinputassistAccept
  succ KeyKbdinputassistAccept = KeyKbdinputassistCancel
  succ KeyKbdinputassistCancel = BtnTriggerHappy1
  succ BtnTriggerHappy1 = BtnTriggerHappy2
  succ BtnTriggerHappy2 = BtnTriggerHappy3
  succ BtnTriggerHappy3 = BtnTriggerHappy4
  succ BtnTriggerHappy4 = BtnTriggerHappy5
  succ BtnTriggerHappy5 = BtnTriggerHappy6
  succ BtnTriggerHappy6 = BtnTriggerHappy7
  succ BtnTriggerHappy7 = BtnTriggerHappy8
  succ BtnTriggerHappy8 = BtnTriggerHappy9
  succ BtnTriggerHappy9 = BtnTriggerHappy10
  succ BtnTriggerHappy10 = BtnTriggerHappy11
  succ BtnTriggerHappy11 = BtnTriggerHappy12
  succ BtnTriggerHappy12 = BtnTriggerHappy13
  succ BtnTriggerHappy13 = BtnTriggerHappy14
  succ BtnTriggerHappy14 = BtnTriggerHappy15
  succ BtnTriggerHappy15 = BtnTriggerHappy16
  succ BtnTriggerHappy16 = BtnTriggerHappy17
  succ BtnTriggerHappy17 = BtnTriggerHappy18
  succ BtnTriggerHappy18 = BtnTriggerHappy19
  succ BtnTriggerHappy19 = BtnTriggerHappy20
  succ BtnTriggerHappy20 = BtnTriggerHappy21
  succ BtnTriggerHappy21 = BtnTriggerHappy22
  succ BtnTriggerHappy22 = BtnTriggerHappy23
  succ BtnTriggerHappy23 = BtnTriggerHappy24
  succ BtnTriggerHappy24 = BtnTriggerHappy25
  succ BtnTriggerHappy25 = BtnTriggerHappy26
  succ BtnTriggerHappy26 = BtnTriggerHappy27
  succ BtnTriggerHappy27 = BtnTriggerHappy28
  succ BtnTriggerHappy28 = BtnTriggerHappy29
  succ BtnTriggerHappy29 = BtnTriggerHappy30
  succ BtnTriggerHappy30 = BtnTriggerHappy31
  succ BtnTriggerHappy31 = BtnTriggerHappy32
  succ BtnTriggerHappy32 = BtnTriggerHappy33
  succ BtnTriggerHappy33 = BtnTriggerHappy34
  succ BtnTriggerHappy34 = BtnTriggerHappy35
  succ BtnTriggerHappy35 = BtnTriggerHappy36
  succ BtnTriggerHappy36 = BtnTriggerHappy37
  succ BtnTriggerHappy37 = BtnTriggerHappy38
  succ BtnTriggerHappy38 = BtnTriggerHappy39
  succ BtnTriggerHappy39 = BtnTriggerHappy40
  succ BtnTriggerHappy40 = error "Key.succ: BtnTriggerHappy40 has no successor"

  pred KeyEsc = KeyReserved
  pred Key1 = KeyEsc
  pred Key2 = Key1
  pred Key3 = Key2
  pred Key4 = Key3
  pred Key5 = Key4
  pred Key6 = Key5
  pred Key7 = Key6
  pred Key8 = Key7
  pred Key9 = Key8
  pred Key0 = Key9
  pred KeyMinus = Key0
  pred KeyEqual = KeyMinus
  pred KeyBackspace = KeyEqual
  pred KeyTab = KeyBackspace
  pred KeyQ = KeyTab
  pred KeyW = KeyQ
  pred KeyE = KeyW
  pred KeyR = KeyE
  pred KeyT = KeyR
  pred KeyY = KeyT
  pred KeyU = KeyY
  pred KeyI = KeyU
  pred KeyO = KeyI
  pred KeyP = KeyO
  pred KeyLeftbrace = KeyP
  pred KeyRightbrace = KeyLeftbrace
  pred KeyEnter = KeyRightbrace
  pred KeyLeftctrl = KeyEnter
  pred KeyA = KeyLeftctrl
  pred KeyS = KeyA
  pred KeyD = KeyS
  pred KeyF = KeyD
  pred KeyG = KeyF
  pred KeyH = KeyG
  pred KeyJ = KeyH
  pred KeyK = KeyJ
  pred KeyL = KeyK
  pred KeySemicolon = KeyL
  pred KeyApostrophe = KeySemicolon
  pred KeyGrave = KeyApostrophe
  pred KeyLeftshift = KeyGrave
  pred KeyBackslash = KeyLeftshift
  pred KeyZ = KeyBackslash
  pred KeyX = KeyZ
  pred KeyC = KeyX
  pred KeyV = KeyC
  pred KeyB = KeyV
  pred KeyN = KeyB
  pred KeyM = KeyN
  pred KeyComma = KeyM
  pred KeyDot = KeyComma
  pred KeySlash = KeyDot
  pred KeyRightshift = KeySlash
  pred KeyKpasterisk = KeyRightshift
  pred KeyLeftalt = KeyKpasterisk
  pred KeySpace = KeyLeftalt
  pred KeyCapslock = KeySpace
  pred KeyF1 = KeyCapslock
  pred KeyF2 = KeyF1
  pred KeyF3 = KeyF2
  pred KeyF4 = KeyF3
  pred KeyF5 = KeyF4
  pred KeyF6 = KeyF5
  pred KeyF7 = KeyF6
  pred KeyF8 = KeyF7
  pred KeyF9 = KeyF8
  pred KeyF10 = KeyF9
  pred KeyNumlock = KeyF10
  pred KeyScrolllock = KeyNumlock
  pred KeyKp7 = KeyScrolllock
  pred KeyKp8 = KeyKp7
  pred KeyKp9 = KeyKp8
  pred KeyKpminus = KeyKp9
  pred KeyKp4 = KeyKpminus
  pred KeyKp5 = KeyKp4
  pred KeyKp6 = KeyKp5
  pred KeyKpplus = KeyKp6
  pred KeyKp1 = KeyKpplus
  pred KeyKp2 = KeyKp1
  pred KeyKp3 = KeyKp2
  pred KeyKp0 = KeyKp3
  pred KeyKpdot = KeyKp0
  pred KeyZenkakuhankaku = KeyKpdot
  pred Key102nd = KeyZenkakuhankaku
  pred KeyF11 = Key102nd
  pred KeyF12 = KeyF11
  pred KeyRo = KeyF12
  pred KeyKatakana = KeyRo
  pred KeyHiragana = KeyKatakana
  pred KeyHenkan = KeyHiragana
  pred KeyKatakanahiragana = KeyHenkan
  pred KeyMuhenkan = KeyKatakanahiragana
  pred KeyKpjpcomma = KeyMuhenkan
  pred KeyKpenter = KeyKpjpcomma
  pred KeyRightctrl = KeyKpenter
  pred KeyKpslash = KeyRightctrl
  pred KeySysrq = KeyKpslash
  pred KeyRightalt = KeySysrq
  pred KeyLinefeed = KeyRightalt
  pred KeyHome = KeyLinefeed
  pred KeyUp = KeyHome
  pred KeyPageup = KeyUp
  pred KeyLeft = KeyPageup
  pred KeyRight = KeyLeft
  pred KeyEnd = KeyRight
  pred KeyDown = KeyEnd
  pred KeyPagedown = KeyDown
  pred KeyInsert = KeyPagedown
  pred KeyDelete = KeyInsert
  pred KeyMacro = KeyDelete
  pred KeyMute = KeyMacro
  pred KeyVolumedown = KeyMute
  pred KeyVolumeup = KeyVolumedown
  pred KeyPower = KeyVolumeup
  pred KeyKpequal = KeyPower
  pred KeyKpplusminus = KeyKpequal
  pred KeyPause = KeyKpplusminus
  pred KeyScale = KeyPause
  pred KeyKpcomma = KeyScale
  pred KeyHangeul = KeyKpcomma
  pred KeyHanja = KeyHangeul
  pred KeyYen = KeyHanja
  pred KeyLeftmeta = KeyYen
  pred KeyRightmeta = KeyLeftmeta
  pred KeyCompose = KeyRightmeta
  pred KeyStop = KeyCompose
  pred KeyAgain = KeyStop
  pred KeyProps = KeyAgain
  pred KeyUndo = KeyProps
  pred KeyFront = KeyUndo
  pred KeyCopy = KeyFront
  pred KeyOpen = KeyCopy
  pred KeyPaste = KeyOpen
  pred KeyFind = KeyPaste
  pred KeyCut = KeyFind
  pred KeyHelp = KeyCut
  pred KeyMenu = KeyHelp
  pred KeyCalc = KeyMenu
  pred KeySetup = KeyCalc
  pred KeySleep = KeySetup
  pred KeyWakeup = KeySleep
  pred KeyFile = KeyWakeup
  pred KeySendfile = KeyFile
  pred KeyDeletefile = KeySendfile
  pred KeyXfer = KeyDeletefile
  pred KeyProg1 = KeyXfer
  pred KeyProg2 = KeyProg1
  pred KeyWww = KeyProg2
  pred KeyMsdos = KeyWww
  pred KeyScreenlock = KeyMsdos
  pred KeyRotateDisplay = KeyScreenlock
  pred KeyCyclewindows = KeyRotateDisplay
  pred KeyMail = KeyCyclewindows
  pred KeyBookmarks = KeyMail
  pred KeyComputer = KeyBookmarks
  pred KeyBack = KeyComputer
  pred KeyForward = KeyBack
  pred KeyClosecd = KeyForward
  pred KeyEjectcd = KeyClosecd
  pred KeyEjectclosecd = KeyEjectcd
  pred KeyNextsong = KeyEjectclosecd
  pred KeyPlaypause = KeyNextsong
  pred KeyPrevioussong = KeyPlaypause
  pred KeyStopcd = KeyPrevioussong
  pred KeyRecord = KeyStopcd
  pred KeyRewind = KeyRecord
  pred KeyPhone = KeyRewind
  pred KeyIso = KeyPhone
  pred KeyConfig = KeyIso
  pred KeyHomepage = KeyConfig
  pred KeyRefresh = KeyHomepage
  pred KeyExit = KeyRefresh
  pred KeyMove = KeyExit
  pred KeyEdit = KeyMove
  pred KeyScrollup = KeyEdit
  pred KeyScrolldown = KeyScrollup
  pred KeyKpleftparen = KeyScrolldown
  pred KeyKprightparen = KeyKpleftparen
  pred KeyNew = KeyKprightparen
  pred KeyRedo = KeyNew
  pred KeyF13 = KeyRedo
  pred KeyF14 = KeyF13
  pred KeyF15 = KeyF14
  pred KeyF16 = KeyF15
  pred KeyF17 = KeyF16
  pred KeyF18 = KeyF17
  pred KeyF19 = KeyF18
  pred KeyF20 = KeyF19
  pred KeyF21 = KeyF20
  pred KeyF22 = KeyF21
  pred KeyF23 = KeyF22
  pred KeyF24 = KeyF23
  pred KeyPlaycd = KeyF24
  pred KeyPausecd = KeyPlaycd
  pred KeyProg3 = KeyPausecd
  pred KeyProg4 = KeyProg3
  pred KeyDashboard = KeyProg4
  pred KeySuspend = KeyDashboard
  pred KeyClose = KeySuspend
  pred KeyPlay = KeyClose
  pred KeyFastforward = KeyPlay
  pred KeyBassboost = KeyFastforward
  pred KeyPrint = KeyBassboost
  pred KeyHp = KeyPrint
  pred KeyCamera = KeyHp
  pred KeySound = KeyCamera
  pred KeyQuestion = KeySound
  pred KeyEmail = KeyQuestion
  pred KeyChat = KeyEmail
  pred KeySearch = KeyChat
  pred KeyConnect = KeySearch
  pred KeyFinance = KeyConnect
  pred KeySport = KeyFinance
  pred KeyShop = KeySport
  pred KeyAlterase = KeyShop
  pred KeyCancel = KeyAlterase
  pred KeyBrightnessdown = KeyCancel
  pred KeyBrightnessup = KeyBrightnessdown
  pred KeyMedia = KeyBrightnessup
  pred KeySwitchvideomode = KeyMedia
  pred KeyKbdillumtoggle = KeySwitchvideomode
  pred KeyKbdillumdown = KeyKbdillumtoggle
  pred KeyKbdillumup = KeyKbdillumdown
  pred KeySend = KeyKbdillumup
  pred KeyReply = KeySend
  pred KeyForwardmail = KeyReply
  pred KeySave = KeyForwardmail
  pred KeyDocuments = KeySave
  pred KeyBattery = KeyDocuments
  pred KeyBluetooth = KeyBattery
  pred KeyWlan = KeyBluetooth
  pred KeyUwb = KeyWlan
  pred KeyUnknown = KeyUwb
  pred KeyVideoNext = KeyUnknown
  pred KeyVideoPrev = KeyVideoNext
  pred KeyBrightnessCycle = KeyVideoPrev
  pred KeyBrightnessAuto = KeyBrightnessCycle
  pred KeyDisplayOff = KeyBrightnessAuto
  pred KeyWwan = KeyDisplayOff
  pred KeyRfkill = KeyWwan
  pred KeyMicmute = KeyRfkill
  pred Btn0 = KeyMicmute
  pred Btn1 = Btn0
  pred Btn2 = Btn1
  pred Btn3 = Btn2
  pred Btn4 = Btn3
  pred Btn5 = Btn4
  pred Btn6 = Btn5
  pred Btn7 = Btn6
  pred Btn8 = Btn7
  pred Btn9 = Btn8
  pred BtnLeft = Btn9
  pred BtnRight = BtnLeft
  pred BtnMiddle = BtnRight
  pred BtnSide = BtnMiddle
  pred BtnExtra = BtnSide
  pred BtnForward = BtnExtra
  pred BtnBack = BtnForward
  pred BtnTask = BtnBack
  pred BtnJoystick = BtnTask
  pred BtnThumb = BtnJoystick
  pred BtnThumb2 = BtnThumb
  pred BtnTop = BtnThumb2
  pred BtnTop2 = BtnTop
  pred BtnPinkie = BtnTop2
  pred BtnBase = BtnPinkie
  pred BtnBase2 = BtnBase
  pred BtnBase3 = BtnBase2
  pred BtnBase4 = BtnBase3
  pred BtnBase5 = BtnBase4
  pred BtnBase6 = BtnBase5
  pred BtnDead = BtnBase6
  pred BtnA = BtnDead
  pred BtnB = BtnA
  pred BtnC = BtnB
  pred BtnX = BtnC
  pred BtnY = BtnX
  pred BtnZ = BtnY
  pred BtnTl = BtnZ
  pred BtnTr = BtnTl
  pred BtnTl2 = BtnTr
  pred BtnTr2 = BtnTl2
  pred BtnSelect = BtnTr2
  pred BtnStart = BtnSelect
  pred BtnMode = BtnStart
  pred BtnThumbl = BtnMode
  pred BtnThumbr = BtnThumbl
  pred BtnToolPen = BtnThumbr
  pred BtnToolRubber = BtnToolPen
  pred BtnToolBrush = BtnToolRubber
  pred BtnToolPencil = BtnToolBrush
  pred BtnToolAirbrush = BtnToolPencil
  pred BtnToolFinger = BtnToolAirbrush
  pred BtnToolMouse = BtnToolFinger
  pred BtnToolLens = BtnToolMouse
  pred BtnToolQuinttap = BtnToolLens
  pred BtnTouch = BtnToolQuinttap
  pred BtnStylus = BtnTouch
  pred BtnStylus2 = BtnStylus
  pred BtnToolDoubletap = BtnStylus2
  pred BtnToolTripletap = BtnToolDoubletap
  pred BtnToolQuadtap = BtnToolTripletap
  pred BtnGearDown = BtnToolQuadtap
  pred BtnGearUp = BtnGearDown
  pred KeyOk = BtnGearUp
  pred KeySelect = KeyOk
  pred KeyGoto = KeySelect
  pred KeyClear = KeyGoto
  pred KeyPower2 = KeyClear
  pred KeyOption = KeyPower2
  pred KeyInfo = KeyOption
  pred KeyTime = KeyInfo
  pred KeyVendor = KeyTime
  pred KeyArchive = KeyVendor
  pred KeyProgram = KeyArchive
  pred KeyChannel = KeyProgram
  pred KeyFavorites = KeyChannel
  pred KeyEpg = KeyFavorites
  pred KeyPvr = KeyEpg
  pred KeyMhp = KeyPvr
  pred KeyLanguage = KeyMhp
  pred KeyTitle = KeyLanguage
  pred KeySubtitle = KeyTitle
  pred KeyAngle = KeySubtitle
  pred KeyZoom = KeyAngle
  pred KeyMode = KeyZoom
  pred KeyKeyboard = KeyMode
  pred KeyScreen = KeyKeyboard
  pred KeyPc = KeyScreen
  pred KeyTv = KeyPc
  pred KeyTv2 = KeyTv
  pred KeyVcr = KeyTv2
  pred KeyVcr2 = KeyVcr
  pred KeySat = KeyVcr2
  pred KeySat2 = KeySat
  pred KeyCd = KeySat2
  pred KeyTape = KeyCd
  pred KeyRadio = KeyTape
  pred KeyTuner = KeyRadio
  pred KeyPlayer = KeyTuner
  pred KeyText = KeyPlayer
  pred KeyDvd = KeyText
  pred KeyAux = KeyDvd
  pred KeyMp3 = KeyAux
  pred KeyAudio = KeyMp3
  pred KeyVideo = KeyAudio
  pred KeyDirectory = KeyVideo
  pred KeyList = KeyDirectory
  pred KeyMemo = KeyList
  pred KeyCalendar = KeyMemo
  pred KeyRed = KeyCalendar
  pred KeyGreen = KeyRed
  pred KeyYellow = KeyGreen
  pred KeyBlue = KeyYellow
  pred KeyChannelup = KeyBlue
  pred KeyChanneldown = KeyChannelup
  pred KeyFirst = KeyChanneldown
  pred KeyLast = KeyFirst
  pred KeyAb = KeyLast
  pred KeyNext = KeyAb
  pred KeyRestart = KeyNext
  pred KeySlow = KeyRestart
  pred KeyShuffle = KeySlow
  pred KeyBreak = KeyShuffle
  pred KeyPrevious = KeyBreak
  pred KeyDigits = KeyPrevious
  pred KeyTeen = KeyDigits
  pred KeyTwen = KeyTeen
  pred KeyVideophone = KeyTwen
  pred KeyGames = KeyVideophone
  pred KeyZoomin = KeyGames
  pred KeyZoomout = KeyZoomin
  pred KeyZoomreset = KeyZoomout
  pred KeyWordprocessor = KeyZoomreset
  pred KeyEditor = KeyWordprocessor
  pred KeySpreadsheet = KeyEditor
  pred KeyGraphicseditor = KeySpreadsheet
  pred KeyPresentation = KeyGraphicseditor
  pred KeyDatabase = KeyPresentation
  pred KeyNews = KeyDatabase
  pred KeyVoicemail = KeyNews
  pred KeyAddressbook = KeyVoicemail
  pred KeyMessenger = KeyAddressbook
  pred KeyDisplaytoggle = KeyMessenger
  pred KeySpellcheck = KeyDisplaytoggle
  pred KeyLogoff = KeySpellcheck
  pred KeyDollar = KeyLogoff
  pred KeyEuro = KeyDollar
  pred KeyFrameback = KeyEuro
  pred KeyFrameforward = KeyFrameback
  pred KeyContextMenu = KeyFrameforward
  pred KeyMediaRepeat = KeyContextMenu
  pred Key10channelsup = KeyMediaRepeat
  pred Key10channelsdown = Key10channelsup
  pred KeyImages = Key10channelsdown
  pred KeyDelEol = KeyImages
  pred KeyDelEos = KeyDelEol
  pred KeyInsLine = KeyDelEos
  pred KeyDelLine = KeyInsLine
  pred KeyFn = KeyDelLine
  pred KeyFnEsc = KeyFn
  pred KeyFnF1 = KeyFnEsc
  pred KeyFnF2 = KeyFnF1
  pred KeyFnF3 = KeyFnF2
  pred KeyFnF4 = KeyFnF3
  pred KeyFnF5 = KeyFnF4
  pred KeyFnF6 = KeyFnF5
  pred KeyFnF7 = KeyFnF6
  pred KeyFnF8 = KeyFnF7
  pred KeyFnF9 = KeyFnF8
  pred KeyFnF10 = KeyFnF9
  pred KeyFnF11 = KeyFnF10
  pred KeyFnF12 = KeyFnF11
  pred KeyFn1 = KeyFnF12
  pred KeyFn2 = KeyFn1
  pred KeyFnD = KeyFn2
  pred KeyFnE = KeyFnD
  pred KeyFnF = KeyFnE
  pred KeyFnS = KeyFnF
  pred KeyFnB = KeyFnS
  pred KeyBrlDot1 = KeyFnB
  pred KeyBrlDot2 = KeyBrlDot1
  pred KeyBrlDot3 = KeyBrlDot2
  pred KeyBrlDot4 = KeyBrlDot3
  pred KeyBrlDot5 = KeyBrlDot4
  pred KeyBrlDot6 = KeyBrlDot5
  pred KeyBrlDot7 = KeyBrlDot6
  pred KeyBrlDot8 = KeyBrlDot7
  pred KeyBrlDot9 = KeyBrlDot8
  pred KeyBrlDot10 = KeyBrlDot9
  pred KeyNumeric0 = KeyBrlDot10
  pred KeyNumeric1 = KeyNumeric0
  pred KeyNumeric2 = KeyNumeric1
  pred KeyNumeric3 = KeyNumeric2
  pred KeyNumeric4 = KeyNumeric3
  pred KeyNumeric5 = KeyNumeric4
  pred KeyNumeric6 = KeyNumeric5
  pred KeyNumeric7 = KeyNumeric6
  pred KeyNumeric8 = KeyNumeric7
  pred KeyNumeric9 = KeyNumeric8
  pred KeyNumericStar = KeyNumeric9
  pred KeyNumericPound = KeyNumericStar
  pred KeyNumericA = KeyNumericPound
  pred KeyNumericB = KeyNumericA
  pred KeyNumericC = KeyNumericB
  pred KeyNumericD = KeyNumericC
  pred KeyCameraFocus = KeyNumericD
  pred KeyWpsButton = KeyCameraFocus
  pred KeyTouchpadToggle = KeyWpsButton
  pred KeyTouchpadOn = KeyTouchpadToggle
  pred KeyTouchpadOff = KeyTouchpadOn
  pred KeyCameraZoomin = KeyTouchpadOff
  pred KeyCameraZoomout = KeyCameraZoomin
  pred KeyCameraUp = KeyCameraZoomout
  pred KeyCameraDown = KeyCameraUp
  pred KeyCameraLeft = KeyCameraDown
  pred KeyCameraRight = KeyCameraLeft
  pred KeyAttendantOn = KeyCameraRight
  pred KeyAttendantOff = KeyAttendantOn
  pred KeyAttendantToggle = KeyAttendantOff
  pred KeyLightsToggle = KeyAttendantToggle
  pred BtnDpadUp = KeyLightsToggle
  pred BtnDpadDown = BtnDpadUp
  pred BtnDpadLeft = BtnDpadDown
  pred BtnDpadRight = BtnDpadLeft
  pred KeyAlsToggle = BtnDpadRight
  pred KeyButtonconfig = KeyAlsToggle
  pred KeyTaskmanager = KeyButtonconfig
  pred KeyJournal = KeyTaskmanager
  pred KeyControlpanel = KeyJournal
  pred KeyAppselect = KeyControlpanel
  pred KeyScreensaver = KeyAppselect
  pred KeyVoicecommand = KeyScreensaver
  pred KeyBrightnessMin = KeyVoicecommand
  pred KeyBrightnessMax = KeyBrightnessMin
  pred KeyKbdinputassistPrev = KeyBrightnessMax
  pred KeyKbdinputassistNext = KeyKbdinputassistPrev
  pred KeyKbdinputassistPrevgroup = KeyKbdinputassistNext
  pred KeyKbdinputassistNextgroup = KeyKbdinputassistPrevgroup
  pred KeyKbdinputassistAccept = KeyKbdinputassistNextgroup
  pred KeyKbdinputassistCancel = KeyKbdinputassistAccept
  pred BtnTriggerHappy1 = KeyKbdinputassistCancel
  pred BtnTriggerHappy2 = BtnTriggerHappy1
  pred BtnTriggerHappy3 = BtnTriggerHappy2
  pred BtnTriggerHappy4 = BtnTriggerHappy3
  pred BtnTriggerHappy5 = BtnTriggerHappy4
  pred BtnTriggerHappy6 = BtnTriggerHappy5
  pred BtnTriggerHappy7 = BtnTriggerHappy6
  pred BtnTriggerHappy8 = BtnTriggerHappy7
  pred BtnTriggerHappy9 = BtnTriggerHappy8
  pred BtnTriggerHappy10 = BtnTriggerHappy9
  pred BtnTriggerHappy11 = BtnTriggerHappy10
  pred BtnTriggerHappy12 = BtnTriggerHappy11
  pred BtnTriggerHappy13 = BtnTriggerHappy12
  pred BtnTriggerHappy14 = BtnTriggerHappy13
  pred BtnTriggerHappy15 = BtnTriggerHappy14
  pred BtnTriggerHappy16 = BtnTriggerHappy15
  pred BtnTriggerHappy17 = BtnTriggerHappy16
  pred BtnTriggerHappy18 = BtnTriggerHappy17
  pred BtnTriggerHappy19 = BtnTriggerHappy18
  pred BtnTriggerHappy20 = BtnTriggerHappy19
  pred BtnTriggerHappy21 = BtnTriggerHappy20
  pred BtnTriggerHappy22 = BtnTriggerHappy21
  pred BtnTriggerHappy23 = BtnTriggerHappy22
  pred BtnTriggerHappy24 = BtnTriggerHappy23
  pred BtnTriggerHappy25 = BtnTriggerHappy24
  pred BtnTriggerHappy26 = BtnTriggerHappy25
  pred BtnTriggerHappy27 = BtnTriggerHappy26
  pred BtnTriggerHappy28 = BtnTriggerHappy27
  pred BtnTriggerHappy29 = BtnTriggerHappy28
  pred BtnTriggerHappy30 = BtnTriggerHappy29
  pred BtnTriggerHappy31 = BtnTriggerHappy30
  pred BtnTriggerHappy32 = BtnTriggerHappy31
  pred BtnTriggerHappy33 = BtnTriggerHappy32
  pred BtnTriggerHappy34 = BtnTriggerHappy33
  pred BtnTriggerHappy35 = BtnTriggerHappy34
  pred BtnTriggerHappy36 = BtnTriggerHappy35
  pred BtnTriggerHappy37 = BtnTriggerHappy36
  pred BtnTriggerHappy38 = BtnTriggerHappy37
  pred BtnTriggerHappy39 = BtnTriggerHappy38
  pred BtnTriggerHappy40 = BtnTriggerHappy39
  pred KeyReserved = error "Key.pred: KeyReserved has no predecessor"

  enumFromTo from to = go from
    where
      end = fromEnum to
      go v = case compare (fromEnum v) end of
                 LT -> v : go (succ v)
                 EQ -> [v]
                 GT -> []

  enumFrom from = enumFromTo from BtnTriggerHappy40

  fromEnum KeyReserved = 0
  fromEnum KeyEsc = 1
  fromEnum Key1 = 2
  fromEnum Key2 = 3
  fromEnum Key3 = 4
  fromEnum Key4 = 5
  fromEnum Key5 = 6
  fromEnum Key6 = 7
  fromEnum Key7 = 8
  fromEnum Key8 = 9
  fromEnum Key9 = 10
  fromEnum Key0 = 11
  fromEnum KeyMinus = 12
  fromEnum KeyEqual = 13
  fromEnum KeyBackspace = 14
  fromEnum KeyTab = 15
  fromEnum KeyQ = 16
  fromEnum KeyW = 17
  fromEnum KeyE = 18
  fromEnum KeyR = 19
  fromEnum KeyT = 20
  fromEnum KeyY = 21
  fromEnum KeyU = 22
  fromEnum KeyI = 23
  fromEnum KeyO = 24
  fromEnum KeyP = 25
  fromEnum KeyLeftbrace = 26
  fromEnum KeyRightbrace = 27
  fromEnum KeyEnter = 28
  fromEnum KeyLeftctrl = 29
  fromEnum KeyA = 30
  fromEnum KeyS = 31
  fromEnum KeyD = 32
  fromEnum KeyF = 33
  fromEnum KeyG = 34
  fromEnum KeyH = 35
  fromEnum KeyJ = 36
  fromEnum KeyK = 37
  fromEnum KeyL = 38
  fromEnum KeySemicolon = 39
  fromEnum KeyApostrophe = 40
  fromEnum KeyGrave = 41
  fromEnum KeyLeftshift = 42
  fromEnum KeyBackslash = 43
  fromEnum KeyZ = 44
  fromEnum KeyX = 45
  fromEnum KeyC = 46
  fromEnum KeyV = 47
  fromEnum KeyB = 48
  fromEnum KeyN = 49
  fromEnum KeyM = 50
  fromEnum KeyComma = 51
  fromEnum KeyDot = 52
  fromEnum KeySlash = 53
  fromEnum KeyRightshift = 54
  fromEnum KeyKpasterisk = 55
  fromEnum KeyLeftalt = 56
  fromEnum KeySpace = 57
  fromEnum KeyCapslock = 58
  fromEnum KeyF1 = 59
  fromEnum KeyF2 = 60
  fromEnum KeyF3 = 61
  fromEnum KeyF4 = 62
  fromEnum KeyF5 = 63
  fromEnum KeyF6 = 64
  fromEnum KeyF7 = 65
  fromEnum KeyF8 = 66
  fromEnum KeyF9 = 67
  fromEnum KeyF10 = 68
  fromEnum KeyNumlock = 69
  fromEnum KeyScrolllock = 70
  fromEnum KeyKp7 = 71
  fromEnum KeyKp8 = 72
  fromEnum KeyKp9 = 73
  fromEnum KeyKpminus = 74
  fromEnum KeyKp4 = 75
  fromEnum KeyKp5 = 76
  fromEnum KeyKp6 = 77
  fromEnum KeyKpplus = 78
  fromEnum KeyKp1 = 79
  fromEnum KeyKp2 = 80
  fromEnum KeyKp3 = 81
  fromEnum KeyKp0 = 82
  fromEnum KeyKpdot = 83
  fromEnum KeyZenkakuhankaku = 85
  fromEnum Key102nd = 86
  fromEnum KeyF11 = 87
  fromEnum KeyF12 = 88
  fromEnum KeyRo = 89
  fromEnum KeyKatakana = 90
  fromEnum KeyHiragana = 91
  fromEnum KeyHenkan = 92
  fromEnum KeyKatakanahiragana = 93
  fromEnum KeyMuhenkan = 94
  fromEnum KeyKpjpcomma = 95
  fromEnum KeyKpenter = 96
  fromEnum KeyRightctrl = 97
  fromEnum KeyKpslash = 98
  fromEnum KeySysrq = 99
  fromEnum KeyRightalt = 100
  fromEnum KeyLinefeed = 101
  fromEnum KeyHome = 102
  fromEnum KeyUp = 103
  fromEnum KeyPageup = 104
  fromEnum KeyLeft = 105
  fromEnum KeyRight = 106
  fromEnum KeyEnd = 107
  fromEnum KeyDown = 108
  fromEnum KeyPagedown = 109
  fromEnum KeyInsert = 110
  fromEnum KeyDelete = 111
  fromEnum KeyMacro = 112
  fromEnum KeyMute = 113
  fromEnum KeyVolumedown = 114
  fromEnum KeyVolumeup = 115
  fromEnum KeyPower = 116
  fromEnum KeyKpequal = 117
  fromEnum KeyKpplusminus = 118
  fromEnum KeyPause = 119
  fromEnum KeyScale = 120
  fromEnum KeyKpcomma = 121
  fromEnum KeyHangeul = 122
  fromEnum KeyHanja = 123
  fromEnum KeyYen = 124
  fromEnum KeyLeftmeta = 125
  fromEnum KeyRightmeta = 126
  fromEnum KeyCompose = 127
  fromEnum KeyStop = 128
  fromEnum KeyAgain = 129
  fromEnum KeyProps = 130
  fromEnum KeyUndo = 131
  fromEnum KeyFront = 132
  fromEnum KeyCopy = 133
  fromEnum KeyOpen = 134
  fromEnum KeyPaste = 135
  fromEnum KeyFind = 136
  fromEnum KeyCut = 137
  fromEnum KeyHelp = 138
  fromEnum KeyMenu = 139
  fromEnum KeyCalc = 140
  fromEnum KeySetup = 141
  fromEnum KeySleep = 142
  fromEnum KeyWakeup = 143
  fromEnum KeyFile = 144
  fromEnum KeySendfile = 145
  fromEnum KeyDeletefile = 146
  fromEnum KeyXfer = 147
  fromEnum KeyProg1 = 148
  fromEnum KeyProg2 = 149
  fromEnum KeyWww = 150
  fromEnum KeyMsdos = 151
  fromEnum KeyScreenlock = 152
  fromEnum KeyRotateDisplay = 153
  fromEnum KeyCyclewindows = 154
  fromEnum KeyMail = 155
  fromEnum KeyBookmarks = 156
  fromEnum KeyComputer = 157
  fromEnum KeyBack = 158
  fromEnum KeyForward = 159
  fromEnum KeyClosecd = 160
  fromEnum KeyEjectcd = 161
  fromEnum KeyEjectclosecd = 162
  fromEnum KeyNextsong = 163
  fromEnum KeyPlaypause = 164
  fromEnum KeyPrevioussong = 165
  fromEnum KeyStopcd = 166
  fromEnum KeyRecord = 167
  fromEnum KeyRewind = 168
  fromEnum KeyPhone = 169
  fromEnum KeyIso = 170
  fromEnum KeyConfig = 171
  fromEnum KeyHomepage = 172
  fromEnum KeyRefresh = 173
  fromEnum KeyExit = 174
  fromEnum KeyMove = 175
  fromEnum KeyEdit = 176
  fromEnum KeyScrollup = 177
  fromEnum KeyScrolldown = 178
  fromEnum KeyKpleftparen = 179
  fromEnum KeyKprightparen = 180
  fromEnum KeyNew = 181
  fromEnum KeyRedo = 182
  fromEnum KeyF13 = 183
  fromEnum KeyF14 = 184
  fromEnum KeyF15 = 185
  fromEnum KeyF16 = 186
  fromEnum KeyF17 = 187
  fromEnum KeyF18 = 188
  fromEnum KeyF19 = 189
  fromEnum KeyF20 = 190
  fromEnum KeyF21 = 191
  fromEnum KeyF22 = 192
  fromEnum KeyF23 = 193
  fromEnum KeyF24 = 194
  fromEnum KeyPlaycd = 200
  fromEnum KeyPausecd = 201
  fromEnum KeyProg3 = 202
  fromEnum KeyProg4 = 203
  fromEnum KeyDashboard = 204
  fromEnum KeySuspend = 205
  fromEnum KeyClose = 206
  fromEnum KeyPlay = 207
  fromEnum KeyFastforward = 208
  fromEnum KeyBassboost = 209
  fromEnum KeyPrint = 210
  fromEnum KeyHp = 211
  fromEnum KeyCamera = 212
  fromEnum KeySound = 213
  fromEnum KeyQuestion = 214
  fromEnum KeyEmail = 215
  fromEnum KeyChat = 216
  fromEnum KeySearch = 217
  fromEnum KeyConnect = 218
  fromEnum KeyFinance = 219
  fromEnum KeySport = 220
  fromEnum KeyShop = 221
  fromEnum KeyAlterase = 222
  fromEnum KeyCancel = 223
  fromEnum KeyBrightnessdown = 224
  fromEnum KeyBrightnessup = 225
  fromEnum KeyMedia = 226
  fromEnum KeySwitchvideomode = 227
  fromEnum KeyKbdillumtoggle = 228
  fromEnum KeyKbdillumdown = 229
  fromEnum KeyKbdillumup = 230
  fromEnum KeySend = 231
  fromEnum KeyReply = 232
  fromEnum KeyForwardmail = 233
  fromEnum KeySave = 234
  fromEnum KeyDocuments = 235
  fromEnum KeyBattery = 236
  fromEnum KeyBluetooth = 237
  fromEnum KeyWlan = 238
  fromEnum KeyUwb = 239
  fromEnum KeyUnknown = 240
  fromEnum KeyVideoNext = 241
  fromEnum KeyVideoPrev = 242
  fromEnum KeyBrightnessCycle = 243
  fromEnum KeyBrightnessAuto = 244
  fromEnum KeyDisplayOff = 245
  fromEnum KeyWwan = 246
  fromEnum KeyRfkill = 247
  fromEnum KeyMicmute = 248
  fromEnum Btn0 = 256
  fromEnum Btn1 = 257
  fromEnum Btn2 = 258
  fromEnum Btn3 = 259
  fromEnum Btn4 = 260
  fromEnum Btn5 = 261
  fromEnum Btn6 = 262
  fromEnum Btn7 = 263
  fromEnum Btn8 = 264
  fromEnum Btn9 = 265
  fromEnum BtnLeft = 272
  fromEnum BtnRight = 273
  fromEnum BtnMiddle = 274
  fromEnum BtnSide = 275
  fromEnum BtnExtra = 276
  fromEnum BtnForward = 277
  fromEnum BtnBack = 278
  fromEnum BtnTask = 279
  fromEnum BtnJoystick = 288
  fromEnum BtnThumb = 289
  fromEnum BtnThumb2 = 290
  fromEnum BtnTop = 291
  fromEnum BtnTop2 = 292
  fromEnum BtnPinkie = 293
  fromEnum BtnBase = 294
  fromEnum BtnBase2 = 295
  fromEnum BtnBase3 = 296
  fromEnum BtnBase4 = 297
  fromEnum BtnBase5 = 298
  fromEnum BtnBase6 = 299
  fromEnum BtnDead = 303
  fromEnum BtnA = 304
  fromEnum BtnB = 305
  fromEnum BtnC = 306
  fromEnum BtnX = 307
  fromEnum BtnY = 308
  fromEnum BtnZ = 309
  fromEnum BtnTl = 310
  fromEnum BtnTr = 311
  fromEnum BtnTl2 = 312
  fromEnum BtnTr2 = 313
  fromEnum BtnSelect = 314
  fromEnum BtnStart = 315
  fromEnum BtnMode = 316
  fromEnum BtnThumbl = 317
  fromEnum BtnThumbr = 318
  fromEnum BtnToolPen = 320
  fromEnum BtnToolRubber = 321
  fromEnum BtnToolBrush = 322
  fromEnum BtnToolPencil = 323
  fromEnum BtnToolAirbrush = 324
  fromEnum BtnToolFinger = 325
  fromEnum BtnToolMouse = 326
  fromEnum BtnToolLens = 327
  fromEnum BtnToolQuinttap = 328
  fromEnum BtnTouch = 330
  fromEnum BtnStylus = 331
  fromEnum BtnStylus2 = 332
  fromEnum BtnToolDoubletap = 333
  fromEnum BtnToolTripletap = 334
  fromEnum BtnToolQuadtap = 335
  fromEnum BtnGearDown = 336
  fromEnum BtnGearUp = 337
  fromEnum KeyOk = 352
  fromEnum KeySelect = 353
  fromEnum KeyGoto = 354
  fromEnum KeyClear = 355
  fromEnum KeyPower2 = 356
  fromEnum KeyOption = 357
  fromEnum KeyInfo = 358
  fromEnum KeyTime = 359
  fromEnum KeyVendor = 360
  fromEnum KeyArchive = 361
  fromEnum KeyProgram = 362
  fromEnum KeyChannel = 363
  fromEnum KeyFavorites = 364
  fromEnum KeyEpg = 365
  fromEnum KeyPvr = 366
  fromEnum KeyMhp = 367
  fromEnum KeyLanguage = 368
  fromEnum KeyTitle = 369
  fromEnum KeySubtitle = 370
  fromEnum KeyAngle = 371
  fromEnum KeyZoom = 372
  fromEnum KeyMode = 373
  fromEnum KeyKeyboard = 374
  fromEnum KeyScreen = 375
  fromEnum KeyPc = 376
  fromEnum KeyTv = 377
  fromEnum KeyTv2 = 378
  fromEnum KeyVcr = 379
  fromEnum KeyVcr2 = 380
  fromEnum KeySat = 381
  fromEnum KeySat2 = 382
  fromEnum KeyCd = 383
  fromEnum KeyTape = 384
  fromEnum KeyRadio = 385
  fromEnum KeyTuner = 386
  fromEnum KeyPlayer = 387
  fromEnum KeyText = 388
  fromEnum KeyDvd = 389
  fromEnum KeyAux = 390
  fromEnum KeyMp3 = 391
  fromEnum KeyAudio = 392
  fromEnum KeyVideo = 393
  fromEnum KeyDirectory = 394
  fromEnum KeyList = 395
  fromEnum KeyMemo = 396
  fromEnum KeyCalendar = 397
  fromEnum KeyRed = 398
  fromEnum KeyGreen = 399
  fromEnum KeyYellow = 400
  fromEnum KeyBlue = 401
  fromEnum KeyChannelup = 402
  fromEnum KeyChanneldown = 403
  fromEnum KeyFirst = 404
  fromEnum KeyLast = 405
  fromEnum KeyAb = 406
  fromEnum KeyNext = 407
  fromEnum KeyRestart = 408
  fromEnum KeySlow = 409
  fromEnum KeyShuffle = 410
  fromEnum KeyBreak = 411
  fromEnum KeyPrevious = 412
  fromEnum KeyDigits = 413
  fromEnum KeyTeen = 414
  fromEnum KeyTwen = 415
  fromEnum KeyVideophone = 416
  fromEnum KeyGames = 417
  fromEnum KeyZoomin = 418
  fromEnum KeyZoomout = 419
  fromEnum KeyZoomreset = 420
  fromEnum KeyWordprocessor = 421
  fromEnum KeyEditor = 422
  fromEnum KeySpreadsheet = 423
  fromEnum KeyGraphicseditor = 424
  fromEnum KeyPresentation = 425
  fromEnum KeyDatabase = 426
  fromEnum KeyNews = 427
  fromEnum KeyVoicemail = 428
  fromEnum KeyAddressbook = 429
  fromEnum KeyMessenger = 430
  fromEnum KeyDisplaytoggle = 431
  fromEnum KeySpellcheck = 432
  fromEnum KeyLogoff = 433
  fromEnum KeyDollar = 434
  fromEnum KeyEuro = 435
  fromEnum KeyFrameback = 436
  fromEnum KeyFrameforward = 437
  fromEnum KeyContextMenu = 438
  fromEnum KeyMediaRepeat = 439
  fromEnum Key10channelsup = 440
  fromEnum Key10channelsdown = 441
  fromEnum KeyImages = 442
  fromEnum KeyDelEol = 448
  fromEnum KeyDelEos = 449
  fromEnum KeyInsLine = 450
  fromEnum KeyDelLine = 451
  fromEnum KeyFn = 464
  fromEnum KeyFnEsc = 465
  fromEnum KeyFnF1 = 466
  fromEnum KeyFnF2 = 467
  fromEnum KeyFnF3 = 468
  fromEnum KeyFnF4 = 469
  fromEnum KeyFnF5 = 470
  fromEnum KeyFnF6 = 471
  fromEnum KeyFnF7 = 472
  fromEnum KeyFnF8 = 473
  fromEnum KeyFnF9 = 474
  fromEnum KeyFnF10 = 475
  fromEnum KeyFnF11 = 476
  fromEnum KeyFnF12 = 477
  fromEnum KeyFn1 = 478
  fromEnum KeyFn2 = 479
  fromEnum KeyFnD = 480
  fromEnum KeyFnE = 481
  fromEnum KeyFnF = 482
  fromEnum KeyFnS = 483
  fromEnum KeyFnB = 484
  fromEnum KeyBrlDot1 = 497
  fromEnum KeyBrlDot2 = 498
  fromEnum KeyBrlDot3 = 499
  fromEnum KeyBrlDot4 = 500
  fromEnum KeyBrlDot5 = 501
  fromEnum KeyBrlDot6 = 502
  fromEnum KeyBrlDot7 = 503
  fromEnum KeyBrlDot8 = 504
  fromEnum KeyBrlDot9 = 505
  fromEnum KeyBrlDot10 = 506
  fromEnum KeyNumeric0 = 512
  fromEnum KeyNumeric1 = 513
  fromEnum KeyNumeric2 = 514
  fromEnum KeyNumeric3 = 515
  fromEnum KeyNumeric4 = 516
  fromEnum KeyNumeric5 = 517
  fromEnum KeyNumeric6 = 518
  fromEnum KeyNumeric7 = 519
  fromEnum KeyNumeric8 = 520
  fromEnum KeyNumeric9 = 521
  fromEnum KeyNumericStar = 522
  fromEnum KeyNumericPound = 523
  fromEnum KeyNumericA = 524
  fromEnum KeyNumericB = 525
  fromEnum KeyNumericC = 526
  fromEnum KeyNumericD = 527
  fromEnum KeyCameraFocus = 528
  fromEnum KeyWpsButton = 529
  fromEnum KeyTouchpadToggle = 530
  fromEnum KeyTouchpadOn = 531
  fromEnum KeyTouchpadOff = 532
  fromEnum KeyCameraZoomin = 533
  fromEnum KeyCameraZoomout = 534
  fromEnum KeyCameraUp = 535
  fromEnum KeyCameraDown = 536
  fromEnum KeyCameraLeft = 537
  fromEnum KeyCameraRight = 538
  fromEnum KeyAttendantOn = 539
  fromEnum KeyAttendantOff = 540
  fromEnum KeyAttendantToggle = 541
  fromEnum KeyLightsToggle = 542
  fromEnum BtnDpadUp = 544
  fromEnum BtnDpadDown = 545
  fromEnum BtnDpadLeft = 546
  fromEnum BtnDpadRight = 547
  fromEnum KeyAlsToggle = 560
  fromEnum KeyButtonconfig = 576
  fromEnum KeyTaskmanager = 577
  fromEnum KeyJournal = 578
  fromEnum KeyControlpanel = 579
  fromEnum KeyAppselect = 580
  fromEnum KeyScreensaver = 581
  fromEnum KeyVoicecommand = 582
  fromEnum KeyBrightnessMin = 592
  fromEnum KeyBrightnessMax = 593
  fromEnum KeyKbdinputassistPrev = 608
  fromEnum KeyKbdinputassistNext = 609
  fromEnum KeyKbdinputassistPrevgroup = 610
  fromEnum KeyKbdinputassistNextgroup = 611
  fromEnum KeyKbdinputassistAccept = 612
  fromEnum KeyKbdinputassistCancel = 613
  fromEnum BtnTriggerHappy1 = 704
  fromEnum BtnTriggerHappy2 = 705
  fromEnum BtnTriggerHappy3 = 706
  fromEnum BtnTriggerHappy4 = 707
  fromEnum BtnTriggerHappy5 = 708
  fromEnum BtnTriggerHappy6 = 709
  fromEnum BtnTriggerHappy7 = 710
  fromEnum BtnTriggerHappy8 = 711
  fromEnum BtnTriggerHappy9 = 712
  fromEnum BtnTriggerHappy10 = 713
  fromEnum BtnTriggerHappy11 = 714
  fromEnum BtnTriggerHappy12 = 715
  fromEnum BtnTriggerHappy13 = 716
  fromEnum BtnTriggerHappy14 = 717
  fromEnum BtnTriggerHappy15 = 718
  fromEnum BtnTriggerHappy16 = 719
  fromEnum BtnTriggerHappy17 = 720
  fromEnum BtnTriggerHappy18 = 721
  fromEnum BtnTriggerHappy19 = 722
  fromEnum BtnTriggerHappy20 = 723
  fromEnum BtnTriggerHappy21 = 724
  fromEnum BtnTriggerHappy22 = 725
  fromEnum BtnTriggerHappy23 = 726
  fromEnum BtnTriggerHappy24 = 727
  fromEnum BtnTriggerHappy25 = 728
  fromEnum BtnTriggerHappy26 = 729
  fromEnum BtnTriggerHappy27 = 730
  fromEnum BtnTriggerHappy28 = 731
  fromEnum BtnTriggerHappy29 = 732
  fromEnum BtnTriggerHappy30 = 733
  fromEnum BtnTriggerHappy31 = 734
  fromEnum BtnTriggerHappy32 = 735
  fromEnum BtnTriggerHappy33 = 736
  fromEnum BtnTriggerHappy34 = 737
  fromEnum BtnTriggerHappy35 = 738
  fromEnum BtnTriggerHappy36 = 739
  fromEnum BtnTriggerHappy37 = 740
  fromEnum BtnTriggerHappy38 = 741
  fromEnum BtnTriggerHappy39 = 742
  fromEnum BtnTriggerHappy40 = 743

  toEnum 0 = KeyReserved
  toEnum 1 = KeyEsc
  toEnum 2 = Key1
  toEnum 3 = Key2
  toEnum 4 = Key3
  toEnum 5 = Key4
  toEnum 6 = Key5
  toEnum 7 = Key6
  toEnum 8 = Key7
  toEnum 9 = Key8
  toEnum 10 = Key9
  toEnum 11 = Key0
  toEnum 12 = KeyMinus
  toEnum 13 = KeyEqual
  toEnum 14 = KeyBackspace
  toEnum 15 = KeyTab
  toEnum 16 = KeyQ
  toEnum 17 = KeyW
  toEnum 18 = KeyE
  toEnum 19 = KeyR
  toEnum 20 = KeyT
  toEnum 21 = KeyY
  toEnum 22 = KeyU
  toEnum 23 = KeyI
  toEnum 24 = KeyO
  toEnum 25 = KeyP
  toEnum 26 = KeyLeftbrace
  toEnum 27 = KeyRightbrace
  toEnum 28 = KeyEnter
  toEnum 29 = KeyLeftctrl
  toEnum 30 = KeyA
  toEnum 31 = KeyS
  toEnum 32 = KeyD
  toEnum 33 = KeyF
  toEnum 34 = KeyG
  toEnum 35 = KeyH
  toEnum 36 = KeyJ
  toEnum 37 = KeyK
  toEnum 38 = KeyL
  toEnum 39 = KeySemicolon
  toEnum 40 = KeyApostrophe
  toEnum 41 = KeyGrave
  toEnum 42 = KeyLeftshift
  toEnum 43 = KeyBackslash
  toEnum 44 = KeyZ
  toEnum 45 = KeyX
  toEnum 46 = KeyC
  toEnum 47 = KeyV
  toEnum 48 = KeyB
  toEnum 49 = KeyN
  toEnum 50 = KeyM
  toEnum 51 = KeyComma
  toEnum 52 = KeyDot
  toEnum 53 = KeySlash
  toEnum 54 = KeyRightshift
  toEnum 55 = KeyKpasterisk
  toEnum 56 = KeyLeftalt
  toEnum 57 = KeySpace
  toEnum 58 = KeyCapslock
  toEnum 59 = KeyF1
  toEnum 60 = KeyF2
  toEnum 61 = KeyF3
  toEnum 62 = KeyF4
  toEnum 63 = KeyF5
  toEnum 64 = KeyF6
  toEnum 65 = KeyF7
  toEnum 66 = KeyF8
  toEnum 67 = KeyF9
  toEnum 68 = KeyF10
  toEnum 69 = KeyNumlock
  toEnum 70 = KeyScrolllock
  toEnum 71 = KeyKp7
  toEnum 72 = KeyKp8
  toEnum 73 = KeyKp9
  toEnum 74 = KeyKpminus
  toEnum 75 = KeyKp4
  toEnum 76 = KeyKp5
  toEnum 77 = KeyKp6
  toEnum 78 = KeyKpplus
  toEnum 79 = KeyKp1
  toEnum 80 = KeyKp2
  toEnum 81 = KeyKp3
  toEnum 82 = KeyKp0
  toEnum 83 = KeyKpdot
  toEnum 85 = KeyZenkakuhankaku
  toEnum 86 = Key102nd
  toEnum 87 = KeyF11
  toEnum 88 = KeyF12
  toEnum 89 = KeyRo
  toEnum 90 = KeyKatakana
  toEnum 91 = KeyHiragana
  toEnum 92 = KeyHenkan
  toEnum 93 = KeyKatakanahiragana
  toEnum 94 = KeyMuhenkan
  toEnum 95 = KeyKpjpcomma
  toEnum 96 = KeyKpenter
  toEnum 97 = KeyRightctrl
  toEnum 98 = KeyKpslash
  toEnum 99 = KeySysrq
  toEnum 100 = KeyRightalt
  toEnum 101 = KeyLinefeed
  toEnum 102 = KeyHome
  toEnum 103 = KeyUp
  toEnum 104 = KeyPageup
  toEnum 105 = KeyLeft
  toEnum 106 = KeyRight
  toEnum 107 = KeyEnd
  toEnum 108 = KeyDown
  toEnum 109 = KeyPagedown
  toEnum 110 = KeyInsert
  toEnum 111 = KeyDelete
  toEnum 112 = KeyMacro
  toEnum 113 = KeyMute
  toEnum 114 = KeyVolumedown
  toEnum 115 = KeyVolumeup
  toEnum 116 = KeyPower
  toEnum 117 = KeyKpequal
  toEnum 118 = KeyKpplusminus
  toEnum 119 = KeyPause
  toEnum 120 = KeyScale
  toEnum 121 = KeyKpcomma
  toEnum 122 = KeyHangeul
  toEnum 123 = KeyHanja
  toEnum 124 = KeyYen
  toEnum 125 = KeyLeftmeta
  toEnum 126 = KeyRightmeta
  toEnum 127 = KeyCompose
  toEnum 128 = KeyStop
  toEnum 129 = KeyAgain
  toEnum 130 = KeyProps
  toEnum 131 = KeyUndo
  toEnum 132 = KeyFront
  toEnum 133 = KeyCopy
  toEnum 134 = KeyOpen
  toEnum 135 = KeyPaste
  toEnum 136 = KeyFind
  toEnum 137 = KeyCut
  toEnum 138 = KeyHelp
  toEnum 139 = KeyMenu
  toEnum 140 = KeyCalc
  toEnum 141 = KeySetup
  toEnum 142 = KeySleep
  toEnum 143 = KeyWakeup
  toEnum 144 = KeyFile
  toEnum 145 = KeySendfile
  toEnum 146 = KeyDeletefile
  toEnum 147 = KeyXfer
  toEnum 148 = KeyProg1
  toEnum 149 = KeyProg2
  toEnum 150 = KeyWww
  toEnum 151 = KeyMsdos
  toEnum 152 = KeyScreenlock
  toEnum 153 = KeyRotateDisplay
  toEnum 154 = KeyCyclewindows
  toEnum 155 = KeyMail
  toEnum 156 = KeyBookmarks
  toEnum 157 = KeyComputer
  toEnum 158 = KeyBack
  toEnum 159 = KeyForward
  toEnum 160 = KeyClosecd
  toEnum 161 = KeyEjectcd
  toEnum 162 = KeyEjectclosecd
  toEnum 163 = KeyNextsong
  toEnum 164 = KeyPlaypause
  toEnum 165 = KeyPrevioussong
  toEnum 166 = KeyStopcd
  toEnum 167 = KeyRecord
  toEnum 168 = KeyRewind
  toEnum 169 = KeyPhone
  toEnum 170 = KeyIso
  toEnum 171 = KeyConfig
  toEnum 172 = KeyHomepage
  toEnum 173 = KeyRefresh
  toEnum 174 = KeyExit
  toEnum 175 = KeyMove
  toEnum 176 = KeyEdit
  toEnum 177 = KeyScrollup
  toEnum 178 = KeyScrolldown
  toEnum 179 = KeyKpleftparen
  toEnum 180 = KeyKprightparen
  toEnum 181 = KeyNew
  toEnum 182 = KeyRedo
  toEnum 183 = KeyF13
  toEnum 184 = KeyF14
  toEnum 185 = KeyF15
  toEnum 186 = KeyF16
  toEnum 187 = KeyF17
  toEnum 188 = KeyF18
  toEnum 189 = KeyF19
  toEnum 190 = KeyF20
  toEnum 191 = KeyF21
  toEnum 192 = KeyF22
  toEnum 193 = KeyF23
  toEnum 194 = KeyF24
  toEnum 200 = KeyPlaycd
  toEnum 201 = KeyPausecd
  toEnum 202 = KeyProg3
  toEnum 203 = KeyProg4
  toEnum 204 = KeyDashboard
  toEnum 205 = KeySuspend
  toEnum 206 = KeyClose
  toEnum 207 = KeyPlay
  toEnum 208 = KeyFastforward
  toEnum 209 = KeyBassboost
  toEnum 210 = KeyPrint
  toEnum 211 = KeyHp
  toEnum 212 = KeyCamera
  toEnum 213 = KeySound
  toEnum 214 = KeyQuestion
  toEnum 215 = KeyEmail
  toEnum 216 = KeyChat
  toEnum 217 = KeySearch
  toEnum 218 = KeyConnect
  toEnum 219 = KeyFinance
  toEnum 220 = KeySport
  toEnum 221 = KeyShop
  toEnum 222 = KeyAlterase
  toEnum 223 = KeyCancel
  toEnum 224 = KeyBrightnessdown
  toEnum 225 = KeyBrightnessup
  toEnum 226 = KeyMedia
  toEnum 227 = KeySwitchvideomode
  toEnum 228 = KeyKbdillumtoggle
  toEnum 229 = KeyKbdillumdown
  toEnum 230 = KeyKbdillumup
  toEnum 231 = KeySend
  toEnum 232 = KeyReply
  toEnum 233 = KeyForwardmail
  toEnum 234 = KeySave
  toEnum 235 = KeyDocuments
  toEnum 236 = KeyBattery
  toEnum 237 = KeyBluetooth
  toEnum 238 = KeyWlan
  toEnum 239 = KeyUwb
  toEnum 240 = KeyUnknown
  toEnum 241 = KeyVideoNext
  toEnum 242 = KeyVideoPrev
  toEnum 243 = KeyBrightnessCycle
  toEnum 244 = KeyBrightnessAuto
  toEnum 245 = KeyDisplayOff
  toEnum 246 = KeyWwan
  toEnum 247 = KeyRfkill
  toEnum 248 = KeyMicmute
  toEnum 256 = Btn0
  toEnum 257 = Btn1
  toEnum 258 = Btn2
  toEnum 259 = Btn3
  toEnum 260 = Btn4
  toEnum 261 = Btn5
  toEnum 262 = Btn6
  toEnum 263 = Btn7
  toEnum 264 = Btn8
  toEnum 265 = Btn9
  toEnum 272 = BtnLeft
  toEnum 273 = BtnRight
  toEnum 274 = BtnMiddle
  toEnum 275 = BtnSide
  toEnum 276 = BtnExtra
  toEnum 277 = BtnForward
  toEnum 278 = BtnBack
  toEnum 279 = BtnTask
  toEnum 288 = BtnJoystick
  toEnum 289 = BtnThumb
  toEnum 290 = BtnThumb2
  toEnum 291 = BtnTop
  toEnum 292 = BtnTop2
  toEnum 293 = BtnPinkie
  toEnum 294 = BtnBase
  toEnum 295 = BtnBase2
  toEnum 296 = BtnBase3
  toEnum 297 = BtnBase4
  toEnum 298 = BtnBase5
  toEnum 299 = BtnBase6
  toEnum 303 = BtnDead
  toEnum 304 = BtnA
  toEnum 305 = BtnB
  toEnum 306 = BtnC
  toEnum 307 = BtnX
  toEnum 308 = BtnY
  toEnum 309 = BtnZ
  toEnum 310 = BtnTl
  toEnum 311 = BtnTr
  toEnum 312 = BtnTl2
  toEnum 313 = BtnTr2
  toEnum 314 = BtnSelect
  toEnum 315 = BtnStart
  toEnum 316 = BtnMode
  toEnum 317 = BtnThumbl
  toEnum 318 = BtnThumbr
  toEnum 320 = BtnToolPen
  toEnum 321 = BtnToolRubber
  toEnum 322 = BtnToolBrush
  toEnum 323 = BtnToolPencil
  toEnum 324 = BtnToolAirbrush
  toEnum 325 = BtnToolFinger
  toEnum 326 = BtnToolMouse
  toEnum 327 = BtnToolLens
  toEnum 328 = BtnToolQuinttap
  toEnum 330 = BtnTouch
  toEnum 331 = BtnStylus
  toEnum 332 = BtnStylus2
  toEnum 333 = BtnToolDoubletap
  toEnum 334 = BtnToolTripletap
  toEnum 335 = BtnToolQuadtap
  toEnum 336 = BtnGearDown
  toEnum 337 = BtnGearUp
  toEnum 352 = KeyOk
  toEnum 353 = KeySelect
  toEnum 354 = KeyGoto
  toEnum 355 = KeyClear
  toEnum 356 = KeyPower2
  toEnum 357 = KeyOption
  toEnum 358 = KeyInfo
  toEnum 359 = KeyTime
  toEnum 360 = KeyVendor
  toEnum 361 = KeyArchive
  toEnum 362 = KeyProgram
  toEnum 363 = KeyChannel
  toEnum 364 = KeyFavorites
  toEnum 365 = KeyEpg
  toEnum 366 = KeyPvr
  toEnum 367 = KeyMhp
  toEnum 368 = KeyLanguage
  toEnum 369 = KeyTitle
  toEnum 370 = KeySubtitle
  toEnum 371 = KeyAngle
  toEnum 372 = KeyZoom
  toEnum 373 = KeyMode
  toEnum 374 = KeyKeyboard
  toEnum 375 = KeyScreen
  toEnum 376 = KeyPc
  toEnum 377 = KeyTv
  toEnum 378 = KeyTv2
  toEnum 379 = KeyVcr
  toEnum 380 = KeyVcr2
  toEnum 381 = KeySat
  toEnum 382 = KeySat2
  toEnum 383 = KeyCd
  toEnum 384 = KeyTape
  toEnum 385 = KeyRadio
  toEnum 386 = KeyTuner
  toEnum 387 = KeyPlayer
  toEnum 388 = KeyText
  toEnum 389 = KeyDvd
  toEnum 390 = KeyAux
  toEnum 391 = KeyMp3
  toEnum 392 = KeyAudio
  toEnum 393 = KeyVideo
  toEnum 394 = KeyDirectory
  toEnum 395 = KeyList
  toEnum 396 = KeyMemo
  toEnum 397 = KeyCalendar
  toEnum 398 = KeyRed
  toEnum 399 = KeyGreen
  toEnum 400 = KeyYellow
  toEnum 401 = KeyBlue
  toEnum 402 = KeyChannelup
  toEnum 403 = KeyChanneldown
  toEnum 404 = KeyFirst
  toEnum 405 = KeyLast
  toEnum 406 = KeyAb
  toEnum 407 = KeyNext
  toEnum 408 = KeyRestart
  toEnum 409 = KeySlow
  toEnum 410 = KeyShuffle
  toEnum 411 = KeyBreak
  toEnum 412 = KeyPrevious
  toEnum 413 = KeyDigits
  toEnum 414 = KeyTeen
  toEnum 415 = KeyTwen
  toEnum 416 = KeyVideophone
  toEnum 417 = KeyGames
  toEnum 418 = KeyZoomin
  toEnum 419 = KeyZoomout
  toEnum 420 = KeyZoomreset
  toEnum 421 = KeyWordprocessor
  toEnum 422 = KeyEditor
  toEnum 423 = KeySpreadsheet
  toEnum 424 = KeyGraphicseditor
  toEnum 425 = KeyPresentation
  toEnum 426 = KeyDatabase
  toEnum 427 = KeyNews
  toEnum 428 = KeyVoicemail
  toEnum 429 = KeyAddressbook
  toEnum 430 = KeyMessenger
  toEnum 431 = KeyDisplaytoggle
  toEnum 432 = KeySpellcheck
  toEnum 433 = KeyLogoff
  toEnum 434 = KeyDollar
  toEnum 435 = KeyEuro
  toEnum 436 = KeyFrameback
  toEnum 437 = KeyFrameforward
  toEnum 438 = KeyContextMenu
  toEnum 439 = KeyMediaRepeat
  toEnum 440 = Key10channelsup
  toEnum 441 = Key10channelsdown
  toEnum 442 = KeyImages
  toEnum 448 = KeyDelEol
  toEnum 449 = KeyDelEos
  toEnum 450 = KeyInsLine
  toEnum 451 = KeyDelLine
  toEnum 464 = KeyFn
  toEnum 465 = KeyFnEsc
  toEnum 466 = KeyFnF1
  toEnum 467 = KeyFnF2
  toEnum 468 = KeyFnF3
  toEnum 469 = KeyFnF4
  toEnum 470 = KeyFnF5
  toEnum 471 = KeyFnF6
  toEnum 472 = KeyFnF7
  toEnum 473 = KeyFnF8
  toEnum 474 = KeyFnF9
  toEnum 475 = KeyFnF10
  toEnum 476 = KeyFnF11
  toEnum 477 = KeyFnF12
  toEnum 478 = KeyFn1
  toEnum 479 = KeyFn2
  toEnum 480 = KeyFnD
  toEnum 481 = KeyFnE
  toEnum 482 = KeyFnF
  toEnum 483 = KeyFnS
  toEnum 484 = KeyFnB
  toEnum 497 = KeyBrlDot1
  toEnum 498 = KeyBrlDot2
  toEnum 499 = KeyBrlDot3
  toEnum 500 = KeyBrlDot4
  toEnum 501 = KeyBrlDot5
  toEnum 502 = KeyBrlDot6
  toEnum 503 = KeyBrlDot7
  toEnum 504 = KeyBrlDot8
  toEnum 505 = KeyBrlDot9
  toEnum 506 = KeyBrlDot10
  toEnum 512 = KeyNumeric0
  toEnum 513 = KeyNumeric1
  toEnum 514 = KeyNumeric2
  toEnum 515 = KeyNumeric3
  toEnum 516 = KeyNumeric4
  toEnum 517 = KeyNumeric5
  toEnum 518 = KeyNumeric6
  toEnum 519 = KeyNumeric7
  toEnum 520 = KeyNumeric8
  toEnum 521 = KeyNumeric9
  toEnum 522 = KeyNumericStar
  toEnum 523 = KeyNumericPound
  toEnum 524 = KeyNumericA
  toEnum 525 = KeyNumericB
  toEnum 526 = KeyNumericC
  toEnum 527 = KeyNumericD
  toEnum 528 = KeyCameraFocus
  toEnum 529 = KeyWpsButton
  toEnum 530 = KeyTouchpadToggle
  toEnum 531 = KeyTouchpadOn
  toEnum 532 = KeyTouchpadOff
  toEnum 533 = KeyCameraZoomin
  toEnum 534 = KeyCameraZoomout
  toEnum 535 = KeyCameraUp
  toEnum 536 = KeyCameraDown
  toEnum 537 = KeyCameraLeft
  toEnum 538 = KeyCameraRight
  toEnum 539 = KeyAttendantOn
  toEnum 540 = KeyAttendantOff
  toEnum 541 = KeyAttendantToggle
  toEnum 542 = KeyLightsToggle
  toEnum 544 = BtnDpadUp
  toEnum 545 = BtnDpadDown
  toEnum 546 = BtnDpadLeft
  toEnum 547 = BtnDpadRight
  toEnum 560 = KeyAlsToggle
  toEnum 576 = KeyButtonconfig
  toEnum 577 = KeyTaskmanager
  toEnum 578 = KeyJournal
  toEnum 579 = KeyControlpanel
  toEnum 580 = KeyAppselect
  toEnum 581 = KeyScreensaver
  toEnum 582 = KeyVoicecommand
  toEnum 592 = KeyBrightnessMin
  toEnum 593 = KeyBrightnessMax
  toEnum 608 = KeyKbdinputassistPrev
  toEnum 609 = KeyKbdinputassistNext
  toEnum 610 = KeyKbdinputassistPrevgroup
  toEnum 611 = KeyKbdinputassistNextgroup
  toEnum 612 = KeyKbdinputassistAccept
  toEnum 613 = KeyKbdinputassistCancel
  toEnum 704 = BtnTriggerHappy1
  toEnum 705 = BtnTriggerHappy2
  toEnum 706 = BtnTriggerHappy3
  toEnum 707 = BtnTriggerHappy4
  toEnum 708 = BtnTriggerHappy5
  toEnum 709 = BtnTriggerHappy6
  toEnum 710 = BtnTriggerHappy7
  toEnum 711 = BtnTriggerHappy8
  toEnum 712 = BtnTriggerHappy9
  toEnum 713 = BtnTriggerHappy10
  toEnum 714 = BtnTriggerHappy11
  toEnum 715 = BtnTriggerHappy12
  toEnum 716 = BtnTriggerHappy13
  toEnum 717 = BtnTriggerHappy14
  toEnum 718 = BtnTriggerHappy15
  toEnum 719 = BtnTriggerHappy16
  toEnum 720 = BtnTriggerHappy17
  toEnum 721 = BtnTriggerHappy18
  toEnum 722 = BtnTriggerHappy19
  toEnum 723 = BtnTriggerHappy20
  toEnum 724 = BtnTriggerHappy21
  toEnum 725 = BtnTriggerHappy22
  toEnum 726 = BtnTriggerHappy23
  toEnum 727 = BtnTriggerHappy24
  toEnum 728 = BtnTriggerHappy25
  toEnum 729 = BtnTriggerHappy26
  toEnum 730 = BtnTriggerHappy27
  toEnum 731 = BtnTriggerHappy28
  toEnum 732 = BtnTriggerHappy29
  toEnum 733 = BtnTriggerHappy30
  toEnum 734 = BtnTriggerHappy31
  toEnum 735 = BtnTriggerHappy32
  toEnum 736 = BtnTriggerHappy33
  toEnum 737 = BtnTriggerHappy34
  toEnum 738 = BtnTriggerHappy35
  toEnum 739 = BtnTriggerHappy36
  toEnum 740 = BtnTriggerHappy37
  toEnum 741 = BtnTriggerHappy38
  toEnum 742 = BtnTriggerHappy39
  toEnum 743 = BtnTriggerHappy40
  toEnum unmatched = error ("Key.toEnum: Cannot match " ++ show unmatched)



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
data RelativeAxis = RelX
                  | RelY
                  | RelZ
                  | RelRx
                  | RelRy
                  | RelRz
                  | RelHwheel
                  | RelDial
                  | RelWheel
                  | RelMisc
                  | RelReserved
                  | RelWheelHiRes
                  | RelHWheelHiRes
  deriving (Bounded,Eq,Ord,Read,Show)
instance Enum RelativeAxis where
  succ RelX = RelY
  succ RelY = RelZ
  succ RelZ = RelRx
  succ RelRx = RelRy
  succ RelRy = RelRz
  succ RelRz = RelHwheel
  succ RelHwheel = RelDial
  succ RelDial = RelWheel
  succ RelWheel = RelMisc
  succ RelMisc = RelReserved
  succ RelReserved = RelWheelHiRes
  succ RelWheelHiRes = RelHWheelHiRes
  succ RelHWheelHiRes = error "RelativeAxis.succ: RelHWheelHiRes has no successor"

  pred RelY = RelX
  pred RelZ = RelY
  pred RelRx = RelZ
  pred RelRy = RelRx
  pred RelRz = RelRy
  pred RelHwheel = RelRz
  pred RelDial = RelHwheel
  pred RelWheel = RelDial
  pred RelMisc = RelWheel
  pred RelReserved = RelMisc
  pred RelWheelHiRes = RelReserved
  pred RelHWheelHiRes = RelWheelHiRes
  pred RelX = error "RelativeAxis.pred: RelX has no predecessor"

  enumFromTo from to = go from
    where
      end = fromEnum to
      go v = case compare (fromEnum v) end of
                 LT -> v : go (succ v)
                 EQ -> [v]
                 GT -> []

  enumFrom from = enumFromTo from RelHWheelHiRes

  fromEnum RelX = 0
  fromEnum RelY = 1
  fromEnum RelZ = 2
  fromEnum RelRx = 3
  fromEnum RelRy = 4
  fromEnum RelRz = 5
  fromEnum RelHwheel = 6
  fromEnum RelDial = 7
  fromEnum RelWheel = 8
  fromEnum RelMisc = 9
  fromEnum RelReserved = 10
  fromEnum RelWheelHiRes = 11
  fromEnum RelHWheelHiRes = 12

  toEnum 0 = RelX
  toEnum 1 = RelY
  toEnum 2 = RelZ
  toEnum 3 = RelRx
  toEnum 4 = RelRy
  toEnum 5 = RelRz
  toEnum 6 = RelHwheel
  toEnum 7 = RelDial
  toEnum 8 = RelWheel
  toEnum 9 = RelMisc
  toEnum 10 = RelReserved
  toEnum 11 = RelWheelHiRes
  toEnum 12 = RelHWheelHiRes
  toEnum unmatched = error ("RelativeAxis.toEnum: Cannot match " ++ show unmatched)



-- | Absolute changes
data AbsoluteAxis = AbsX
                  | AbsY
                  | AbsZ
                  | AbsRx
                  | AbsRy
                  | AbsRz
                  | AbsThrottle
                  | AbsRudder
                  | AbsWheel
                  | AbsGas
                  | AbsBrake
                  | AbsHat0x
                  | AbsHat0y
                  | AbsHat1x
                  | AbsHat1y
                  | AbsHat2x
                  | AbsHat2y
                  | AbsHat3x
                  | AbsHat3y
                  | AbsPressure
                  | AbsDistance
                  | AbsTiltX
                  | AbsTiltY
                  | AbsToolWidth
                  | AbsVolume
                  | AbsMisc
                  | AbsReserved
                  | AbsMtSlot
                  | AbsMtTouchMajor
                  | AbsMtTouchMinor
                  | AbsMtWidthMajor
                  | AbsMtWidthMinor
                  | AbsMtOrientation
                  | AbsMtPositionX
                  | AbsMtPositionY
                  | AbsMtToolType
                  | AbsMtBlobId
                  | AbsMtTrackingId
                  | AbsMtPressure
                  | AbsMtDistance
                  | AbsMtToolX
                  | AbsMtToolY
  deriving (Bounded,Eq,Ord,Read,Show)
instance Enum AbsoluteAxis where
  succ AbsX = AbsY
  succ AbsY = AbsZ
  succ AbsZ = AbsRx
  succ AbsRx = AbsRy
  succ AbsRy = AbsRz
  succ AbsRz = AbsThrottle
  succ AbsThrottle = AbsRudder
  succ AbsRudder = AbsWheel
  succ AbsWheel = AbsGas
  succ AbsGas = AbsBrake
  succ AbsBrake = AbsHat0x
  succ AbsHat0x = AbsHat0y
  succ AbsHat0y = AbsHat1x
  succ AbsHat1x = AbsHat1y
  succ AbsHat1y = AbsHat2x
  succ AbsHat2x = AbsHat2y
  succ AbsHat2y = AbsHat3x
  succ AbsHat3x = AbsHat3y
  succ AbsHat3y = AbsPressure
  succ AbsPressure = AbsDistance
  succ AbsDistance = AbsTiltX
  succ AbsTiltX = AbsTiltY
  succ AbsTiltY = AbsToolWidth
  succ AbsToolWidth = AbsVolume
  succ AbsVolume = AbsMisc
  succ AbsMisc = AbsReserved
  succ AbsReserved = AbsMtSlot
  succ AbsMtSlot = AbsMtTouchMajor
  succ AbsMtTouchMajor = AbsMtTouchMinor
  succ AbsMtTouchMinor = AbsMtWidthMajor
  succ AbsMtWidthMajor = AbsMtWidthMinor
  succ AbsMtWidthMinor = AbsMtOrientation
  succ AbsMtOrientation = AbsMtPositionX
  succ AbsMtPositionX = AbsMtPositionY
  succ AbsMtPositionY = AbsMtToolType
  succ AbsMtToolType = AbsMtBlobId
  succ AbsMtBlobId = AbsMtTrackingId
  succ AbsMtTrackingId = AbsMtPressure
  succ AbsMtPressure = AbsMtDistance
  succ AbsMtDistance = AbsMtToolX
  succ AbsMtToolX = AbsMtToolY
  succ AbsMtToolY = error "AbsoluteAxis.succ: AbsMtToolY has no successor"

  pred AbsY = AbsX
  pred AbsZ = AbsY
  pred AbsRx = AbsZ
  pred AbsRy = AbsRx
  pred AbsRz = AbsRy
  pred AbsThrottle = AbsRz
  pred AbsRudder = AbsThrottle
  pred AbsWheel = AbsRudder
  pred AbsGas = AbsWheel
  pred AbsBrake = AbsGas
  pred AbsHat0x = AbsBrake
  pred AbsHat0y = AbsHat0x
  pred AbsHat1x = AbsHat0y
  pred AbsHat1y = AbsHat1x
  pred AbsHat2x = AbsHat1y
  pred AbsHat2y = AbsHat2x
  pred AbsHat3x = AbsHat2y
  pred AbsHat3y = AbsHat3x
  pred AbsPressure = AbsHat3y
  pred AbsDistance = AbsPressure
  pred AbsTiltX = AbsDistance
  pred AbsTiltY = AbsTiltX
  pred AbsToolWidth = AbsTiltY
  pred AbsVolume = AbsToolWidth
  pred AbsMisc = AbsVolume
  pred AbsReserved = AbsMisc
  pred AbsMtSlot = AbsReserved
  pred AbsMtTouchMajor = AbsMtSlot
  pred AbsMtTouchMinor = AbsMtTouchMajor
  pred AbsMtWidthMajor = AbsMtTouchMinor
  pred AbsMtWidthMinor = AbsMtWidthMajor
  pred AbsMtOrientation = AbsMtWidthMinor
  pred AbsMtPositionX = AbsMtOrientation
  pred AbsMtPositionY = AbsMtPositionX
  pred AbsMtToolType = AbsMtPositionY
  pred AbsMtBlobId = AbsMtToolType
  pred AbsMtTrackingId = AbsMtBlobId
  pred AbsMtPressure = AbsMtTrackingId
  pred AbsMtDistance = AbsMtPressure
  pred AbsMtToolX = AbsMtDistance
  pred AbsMtToolY = AbsMtToolX
  pred AbsX = error "AbsoluteAxis.pred: AbsX has no predecessor"

  enumFromTo from to = go from
    where
      end = fromEnum to
      go v = case compare (fromEnum v) end of
                 LT -> v : go (succ v)
                 EQ -> [v]
                 GT -> []

  enumFrom from = enumFromTo from AbsMtToolY

  fromEnum AbsX = 0
  fromEnum AbsY = 1
  fromEnum AbsZ = 2
  fromEnum AbsRx = 3
  fromEnum AbsRy = 4
  fromEnum AbsRz = 5
  fromEnum AbsThrottle = 6
  fromEnum AbsRudder = 7
  fromEnum AbsWheel = 8
  fromEnum AbsGas = 9
  fromEnum AbsBrake = 10
  fromEnum AbsHat0x = 16
  fromEnum AbsHat0y = 17
  fromEnum AbsHat1x = 18
  fromEnum AbsHat1y = 19
  fromEnum AbsHat2x = 20
  fromEnum AbsHat2y = 21
  fromEnum AbsHat3x = 22
  fromEnum AbsHat3y = 23
  fromEnum AbsPressure = 24
  fromEnum AbsDistance = 25
  fromEnum AbsTiltX = 26
  fromEnum AbsTiltY = 27
  fromEnum AbsToolWidth = 28
  fromEnum AbsVolume = 32
  fromEnum AbsMisc = 40
  fromEnum AbsReserved = 46
  fromEnum AbsMtSlot = 47
  fromEnum AbsMtTouchMajor = 48
  fromEnum AbsMtTouchMinor = 49
  fromEnum AbsMtWidthMajor = 50
  fromEnum AbsMtWidthMinor = 51
  fromEnum AbsMtOrientation = 52
  fromEnum AbsMtPositionX = 53
  fromEnum AbsMtPositionY = 54
  fromEnum AbsMtToolType = 55
  fromEnum AbsMtBlobId = 56
  fromEnum AbsMtTrackingId = 57
  fromEnum AbsMtPressure = 58
  fromEnum AbsMtDistance = 59
  fromEnum AbsMtToolX = 60
  fromEnum AbsMtToolY = 61

  toEnum 0 = AbsX
  toEnum 1 = AbsY
  toEnum 2 = AbsZ
  toEnum 3 = AbsRx
  toEnum 4 = AbsRy
  toEnum 5 = AbsRz
  toEnum 6 = AbsThrottle
  toEnum 7 = AbsRudder
  toEnum 8 = AbsWheel
  toEnum 9 = AbsGas
  toEnum 10 = AbsBrake
  toEnum 16 = AbsHat0x
  toEnum 17 = AbsHat0y
  toEnum 18 = AbsHat1x
  toEnum 19 = AbsHat1y
  toEnum 20 = AbsHat2x
  toEnum 21 = AbsHat2y
  toEnum 22 = AbsHat3x
  toEnum 23 = AbsHat3y
  toEnum 24 = AbsPressure
  toEnum 25 = AbsDistance
  toEnum 26 = AbsTiltX
  toEnum 27 = AbsTiltY
  toEnum 28 = AbsToolWidth
  toEnum 32 = AbsVolume
  toEnum 40 = AbsMisc
  toEnum 46 = AbsReserved
  toEnum 47 = AbsMtSlot
  toEnum 48 = AbsMtTouchMajor
  toEnum 49 = AbsMtTouchMinor
  toEnum 50 = AbsMtWidthMajor
  toEnum 51 = AbsMtWidthMinor
  toEnum 52 = AbsMtOrientation
  toEnum 53 = AbsMtPositionX
  toEnum 54 = AbsMtPositionY
  toEnum 55 = AbsMtToolType
  toEnum 56 = AbsMtBlobId
  toEnum 57 = AbsMtTrackingId
  toEnum 58 = AbsMtPressure
  toEnum 59 = AbsMtDistance
  toEnum 60 = AbsMtToolX
  toEnum 61 = AbsMtToolY
  toEnum unmatched = error ("AbsoluteAxis.toEnum: Cannot match " ++ show unmatched)



-- | Stateful binary switches
data SwitchEvent = SwLid
                 | SwTabletMode
                 | SwHeadphoneInsert
                 | SwRfkillAll
                 | SwRadio
                 | SwMicrophoneInsert
                 | SwDock
                 | SwLineoutInsert
                 | SwJackPhysicalInsert
                 | SwVideooutInsert
                 | SwCameraLensCover
                 | SwKeypadSlide
                 | SwFrontProximity
                 | SwRotateLock
                 | SwLineinInsert
                 | SwMuteDevice
  deriving (Bounded,Eq,Ord,Read,Show)
instance Enum SwitchEvent where
  succ SwLid = SwTabletMode
  succ SwTabletMode = SwHeadphoneInsert
  succ SwHeadphoneInsert = SwRfkillAll
  succ SwRfkillAll = SwMicrophoneInsert
  succ SwRadio = SwMicrophoneInsert
  succ SwMicrophoneInsert = SwDock
  succ SwDock = SwLineoutInsert
  succ SwLineoutInsert = SwJackPhysicalInsert
  succ SwJackPhysicalInsert = SwVideooutInsert
  succ SwVideooutInsert = SwCameraLensCover
  succ SwCameraLensCover = SwKeypadSlide
  succ SwKeypadSlide = SwFrontProximity
  succ SwFrontProximity = SwRotateLock
  succ SwRotateLock = SwLineinInsert
  succ SwLineinInsert = SwMuteDevice
  succ SwMuteDevice = error "SwitchEvent.succ: SwMuteDevice has no successor"

  pred SwTabletMode = SwLid
  pred SwHeadphoneInsert = SwTabletMode
  pred SwRfkillAll = SwHeadphoneInsert
  pred SwRadio = SwHeadphoneInsert
  pred SwMicrophoneInsert = SwRfkillAll
  pred SwDock = SwMicrophoneInsert
  pred SwLineoutInsert = SwDock
  pred SwJackPhysicalInsert = SwLineoutInsert
  pred SwVideooutInsert = SwJackPhysicalInsert
  pred SwCameraLensCover = SwVideooutInsert
  pred SwKeypadSlide = SwCameraLensCover
  pred SwFrontProximity = SwKeypadSlide
  pred SwRotateLock = SwFrontProximity
  pred SwLineinInsert = SwRotateLock
  pred SwMuteDevice = SwLineinInsert
  pred SwLid = error "SwitchEvent.pred: SwLid has no predecessor"

  enumFromTo from to = go from
    where
      end = fromEnum to
      go v = case compare (fromEnum v) end of
                 LT -> v : go (succ v)
                 EQ -> [v]
                 GT -> []

  enumFrom from = enumFromTo from SwMuteDevice

  fromEnum SwLid = 0
  fromEnum SwTabletMode = 1
  fromEnum SwHeadphoneInsert = 2
  fromEnum SwRfkillAll = 3
  fromEnum SwRadio = 3
  fromEnum SwMicrophoneInsert = 4
  fromEnum SwDock = 5
  fromEnum SwLineoutInsert = 6
  fromEnum SwJackPhysicalInsert = 7
  fromEnum SwVideooutInsert = 8
  fromEnum SwCameraLensCover = 9
  fromEnum SwKeypadSlide = 10
  fromEnum SwFrontProximity = 11
  fromEnum SwRotateLock = 12
  fromEnum SwLineinInsert = 13
  fromEnum SwMuteDevice = 14

  toEnum 0 = SwLid
  toEnum 1 = SwTabletMode
  toEnum 2 = SwHeadphoneInsert
  toEnum 3 = SwRfkillAll
  toEnum 4 = SwMicrophoneInsert
  toEnum 5 = SwDock
  toEnum 6 = SwLineoutInsert
  toEnum 7 = SwJackPhysicalInsert
  toEnum 8 = SwVideooutInsert
  toEnum 9 = SwCameraLensCover
  toEnum 10 = SwKeypadSlide
  toEnum 11 = SwFrontProximity
  toEnum 12 = SwRotateLock
  toEnum 13 = SwLineinInsert
  toEnum 14 = SwMuteDevice
  toEnum unmatched = error ("SwitchEvent.toEnum: Cannot match " ++ show unmatched)



-- | Miscellaneous
data MiscEvent = MscSerial
               | MscPulseled
               | MscGesture
               | MscRaw
               | MscScan
               | MscTimestamp
  deriving (Bounded,Eq,Ord,Read,Show)
instance Enum MiscEvent where
  succ MscSerial = MscPulseled
  succ MscPulseled = MscGesture
  succ MscGesture = MscRaw
  succ MscRaw = MscScan
  succ MscScan = MscTimestamp
  succ MscTimestamp = error "MiscEvent.succ: MscTimestamp has no successor"

  pred MscPulseled = MscSerial
  pred MscGesture = MscPulseled
  pred MscRaw = MscGesture
  pred MscScan = MscRaw
  pred MscTimestamp = MscScan
  pred MscSerial = error "MiscEvent.pred: MscSerial has no predecessor"

  enumFromTo from to = go from
    where
      end = fromEnum to
      go v = case compare (fromEnum v) end of
                 LT -> v : go (succ v)
                 EQ -> [v]
                 GT -> []

  enumFrom from = enumFromTo from MscTimestamp

  fromEnum MscSerial = 0
  fromEnum MscPulseled = 1
  fromEnum MscGesture = 2
  fromEnum MscRaw = 3
  fromEnum MscScan = 4
  fromEnum MscTimestamp = 5

  toEnum 0 = MscSerial
  toEnum 1 = MscPulseled
  toEnum 2 = MscGesture
  toEnum 3 = MscRaw
  toEnum 4 = MscScan
  toEnum 5 = MscTimestamp
  toEnum unmatched = error ("MiscEvent.toEnum: Cannot match " ++ show unmatched)



-- | LEDs
data LEDEvent = LedNuml
              | LedCapsl
              | LedScrolll
              | LedCompose
              | LedKana
              | LedSleep
              | LedSuspend
              | LedMute
              | LedMisc
              | LedMail
              | LedCharging
  deriving (Bounded,Eq,Ord,Read,Show)
instance Enum LEDEvent where
  succ LedNuml = LedCapsl
  succ LedCapsl = LedScrolll
  succ LedScrolll = LedCompose
  succ LedCompose = LedKana
  succ LedKana = LedSleep
  succ LedSleep = LedSuspend
  succ LedSuspend = LedMute
  succ LedMute = LedMisc
  succ LedMisc = LedMail
  succ LedMail = LedCharging
  succ LedCharging = error "LEDEvent.succ: LedCharging has no successor"

  pred LedCapsl = LedNuml
  pred LedScrolll = LedCapsl
  pred LedCompose = LedScrolll
  pred LedKana = LedCompose
  pred LedSleep = LedKana
  pred LedSuspend = LedSleep
  pred LedMute = LedSuspend
  pred LedMisc = LedMute
  pred LedMail = LedMisc
  pred LedCharging = LedMail
  pred LedNuml = error "LEDEvent.pred: LedNuml has no predecessor"

  enumFromTo from to = go from
    where
      end = fromEnum to
      go v = case compare (fromEnum v) end of
                 LT -> v : go (succ v)
                 EQ -> [v]
                 GT -> []

  enumFrom from = enumFromTo from LedCharging

  fromEnum LedNuml = 0
  fromEnum LedCapsl = 1
  fromEnum LedScrolll = 2
  fromEnum LedCompose = 3
  fromEnum LedKana = 4
  fromEnum LedSleep = 5
  fromEnum LedSuspend = 6
  fromEnum LedMute = 7
  fromEnum LedMisc = 8
  fromEnum LedMail = 9
  fromEnum LedCharging = 10

  toEnum 0 = LedNuml
  toEnum 1 = LedCapsl
  toEnum 2 = LedScrolll
  toEnum 3 = LedCompose
  toEnum 4 = LedKana
  toEnum 5 = LedSleep
  toEnum 6 = LedSuspend
  toEnum 7 = LedMute
  toEnum 8 = LedMisc
  toEnum 9 = LedMail
  toEnum 10 = LedCharging
  toEnum unmatched = error ("LEDEvent.toEnum: Cannot match " ++ show unmatched)



-- | Specifying autorepeating events
data RepeatEvent = RepDelay
                 | RepPeriod
  deriving (Bounded,Eq,Ord,Read,Show)
instance Enum RepeatEvent where
  succ RepDelay = RepPeriod
  succ RepPeriod = error "RepeatEvent.succ: RepPeriod has no successor"

  pred RepPeriod = RepDelay
  pred RepDelay = error "RepeatEvent.pred: RepDelay has no predecessor"

  enumFromTo from to = go from
    where
      end = fromEnum to
      go v = case compare (fromEnum v) end of
                 LT -> v : go (succ v)
                 EQ -> [v]
                 GT -> []

  enumFrom from = enumFromTo from RepPeriod

  fromEnum RepDelay = 0
  fromEnum RepPeriod = 1

  toEnum 0 = RepDelay
  toEnum 1 = RepPeriod
  toEnum unmatched = error ("RepeatEvent.toEnum: Cannot match " ++ show unmatched)



-- | For simple sound output devices
data SoundEvent = SndClick
                | SndBell
                | SndTone
  deriving (Bounded,Eq,Ord,Read,Show)
instance Enum SoundEvent where
  succ SndClick = SndBell
  succ SndBell = SndTone
  succ SndTone = error "SoundEvent.succ: SndTone has no successor"

  pred SndBell = SndClick
  pred SndTone = SndBell
  pred SndClick = error "SoundEvent.pred: SndClick has no predecessor"

  enumFromTo from to = go from
    where
      end = fromEnum to
      go v = case compare (fromEnum v) end of
                 LT -> v : go (succ v)
                 EQ -> [v]
                 GT -> []

  enumFrom from = enumFromTo from SndTone

  fromEnum SndClick = 0
  fromEnum SndBell = 1
  fromEnum SndTone = 2

  toEnum 0 = SndClick
  toEnum 1 = SndBell
  toEnum 2 = SndTone
  toEnum unmatched = error ("SoundEvent.toEnum: Cannot match " ++ show unmatched)



-- | Device properties
data DeviceProperty = InputPropPointer
                    | InputPropDirect
                    | InputPropButtonpad
                    | InputPropSemiMt
                    | InputPropTopbuttonpad
                    | InputPropPointingStick
                    | InputPropAccelerometer
  deriving (Bounded,Eq,Ord,Read,Show)
instance Enum DeviceProperty where
  succ InputPropPointer = InputPropDirect
  succ InputPropDirect = InputPropButtonpad
  succ InputPropButtonpad = InputPropSemiMt
  succ InputPropSemiMt = InputPropTopbuttonpad
  succ InputPropTopbuttonpad = InputPropPointingStick
  succ InputPropPointingStick = InputPropAccelerometer
  succ InputPropAccelerometer = error "DeviceProperty.succ: InputPropAccelerometer has no successor"

  pred InputPropDirect = InputPropPointer
  pred InputPropButtonpad = InputPropDirect
  pred InputPropSemiMt = InputPropButtonpad
  pred InputPropTopbuttonpad = InputPropSemiMt
  pred InputPropPointingStick = InputPropTopbuttonpad
  pred InputPropAccelerometer = InputPropPointingStick
  pred InputPropPointer = error "DeviceProperty.pred: InputPropPointer has no predecessor"

  enumFromTo from to = go from
    where
      end = fromEnum to
      go v = case compare (fromEnum v) end of
                 LT -> v : go (succ v)
                 EQ -> [v]
                 GT -> []

  enumFrom from = enumFromTo from InputPropAccelerometer

  fromEnum InputPropPointer = 0
  fromEnum InputPropDirect = 1
  fromEnum InputPropButtonpad = 2
  fromEnum InputPropSemiMt = 3
  fromEnum InputPropTopbuttonpad = 4
  fromEnum InputPropPointingStick = 5
  fromEnum InputPropAccelerometer = 6

  toEnum 0 = InputPropPointer
  toEnum 1 = InputPropDirect
  toEnum 2 = InputPropButtonpad
  toEnum 3 = InputPropSemiMt
  toEnum 4 = InputPropTopbuttonpad
  toEnum 5 = InputPropPointingStick
  toEnum 6 = InputPropAccelerometer
  toEnum unmatched = error ("DeviceProperty.toEnum: Cannot match " ++ show unmatched)


