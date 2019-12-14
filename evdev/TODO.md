* Documentation
** Get listed under 'bindings' at https://www.freedesktop.org/software/libevdev/doc/latest/
** note at least that some Enum instances are odd
** haddock
*** gaps, and non-injectivity of fromEnum

* Primed versions of functions to allow greater flexibility
** allowing callbacks for eg. when new device found
*** newDevice
*** readEvents

* Module separation
** Namespace (large enums)
** separate high- and low-level

* Future proofing / cross platform
** Changes to evdev / libevdev

* Include examples
** Minimal evtest clone

* names
** should I add extra capitals to #define'd constants

* low-level (non-c2hs) FFI stuff
** add functionality to c2hs and PR
*** getTime (nested structs)
*** nextEvent ('+' without discarding return value)
*** default marshaller for ByteString (LowLevel.deviceName)

* FF, FFStatus, PWR
** find documentation on codes
** test

* cleanup
** libevdev_free and closing fd

* timeout / backoff on handleBoolRetry

* blocking / sudo issue

* we want an association between event types and Event data constructors (type families? DataKinds?)
** otherwise just function Event -> EventType

** showTime in Show Event is a real hack

* direct (Int -> Bytestring) for prettyEvent (bytestring-show package pulls in an awful lot of dependencies)

proper markdown

github documentation

_ in long numeric literals

devicePath doesn't show on hackage (maybe export differently)


throw errors in makeDevices?
