# Revision history for evdev

## 2.3.1 -- 01-09-2023
* Migrate to `unix-2.8`.

## 2.3.0 -- 01-09-2023
* Add `deviceOptsFromEvents` for more easily creating UInput devices with desired capabilities.
* Add some extra `Read` instances.
* Fix duplicate keys by using pattern synonyms.

## 2.2.0 -- 05-11-2022
* More bindings:
    * `deviceAbsAxis`
    * `REL_WHEEL_HI_RES` and `REL_HWHEEL_HI_RES`
    * `setDeviceLED`
* Open devices with write permissions by default. This is primarily in order to make `setDeviceLED` easier to use.
* Export event types from `Uinput` module. This means that that module can actually reasonably be used without also importing `Evdev` and selectively hiding things.
* Wrap results of `deviceUniq` and `devicePhys` in `Maybe`, to avoid possibly dereferencing null pointers.

## 2.1.0 -- 2021-02-12
* Add `Evdev.Uinput` module, for creating virtual devices.
* More functions for querying device properties.
* Remove some invalid instances for `EventCode` and `EventValue`.
* Make it possible for user to create a device from a specified file descriptor.
    * Opens up non-blocking IO, amongst other possibilities.

## 2.0.0.0 -- 2020-05-30
* This is really what should have been the `1.0` release, had I had a better understanding of the PVP when starting off.
* Split Streamly integration into a separate package, `evdev-streamly`.
* Safety improvements - `UnknownEvent` is used to handle a number of what were previously potential crashes.
* Lots of other, minor improvements, including simplifying the API in places, and exposing lower-level functionality where it is likely to be useful.

## 1.3.0.0 -- 2020-03-03
* Fix memory management issues and file descriptor leaks.
* Rename 'ReadFlags' to 'ReadFlag' as the type clearly represents just one flag.

## 1.2.0.0 -- 2019-12-19
* Add `newDevices` stream.
* More robust error handling when reading from multiple devices.

## 1.1.0.1 -- 2019-12-18
* Add `pkgconfig-depends` field to cabal file.

## 1.1.0.0 -- 2019-12-16
* Hide LowLevel.

## 1.0.0.0 -- 2019-12-15
* First stable release.

## 0.1.0.0 -- 2019-07-20
* First version. Released on an unsuspecting world.
