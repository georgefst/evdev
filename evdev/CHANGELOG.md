# Revision history for evdev

## 0.1.0.0 -- 2019-07-20

* First version. Released on an unsuspecting world.

## 1.0.0.0 -- 2019-12-15

* First stable release.

## 1.1.0.0 -- 2019-12-16

* Hide LowLevel.

## 1.1.0.1 -- 2019-12-18

* Add `pkgconfig-depends` field to cabal file.

## 1.2.0.0 -- 2019-12-19

* Add `newDevices` stream.
* More robust error handling when reading from multiple devices.

## 1.3.0.0 -- 2020-03-03

* Fix memory management issues and file descriptor leaks.
* Rename 'ReadFlags' to 'ReadFlag' as the type clearly represents just one flag.
