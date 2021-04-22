# Revision history for evdev

## unreleased
* Bug fix: Wrap results of `deviceUniq` and `devicePhys` in `Maybe`, to avoid possibly dereferencing null pointers.

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
