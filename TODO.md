* Documentation
** Get listed under 'bindings' at https://www.freedesktop.org/software/libevdev/doc/latest/
** note at least that some Enum instances are odd
*** gaps, and non-injectivity of fromEnum

* Primed versions of functions to allow greater flexibility
** newDevice
** readEvents

* Module separation
** Namespace (large enums)
** separate high- and low-level

* Future proofing / cross platform
** Changes to evdev / libevdev

* Include examples
** Minimal evtest clone

* names
** should I add extra capitals to #define'd constants

* remove some of the weirder dependencies

* check haskell int is at least as big is all the C ones

* low-level (non-c2hs) FFI stuff
** add functionality to c2hs and PR
*** getTime (nested structs)
*** nextEvent ('+' without discarding return value)

* all functionality of old evdev haskell library
** device name

* FF, FFStatus, PWR
** find documentation on codes
** test
