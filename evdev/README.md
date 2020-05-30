Evdev
=====

This library provides access to the Linux [evdev](https://en.wikipedia.org/wiki/Evdev) interface, for reading input events from devices. It uses [c2hs](https://github.com/haskell/c2hs/wiki/User-Guide)-generated bindings to [libevdev](https://www.freedesktop.org/wiki/Software/libevdev/), which should be available on almost any modern Linux distro.

Getting started
---------------
Your user will need to be a member of the `input` group in order to read from devices. Try `usermod -a -G input [username]`.

If you wish to make use of concurrency, e.g. to use functions like `allEvents`, be sure to pass the option `-threaded` to GHC, in order to enable the threaded runtime.

The [evdev-examples](https://github.com/georgefst/evdev/tree/master/evdev-examples) folder contains a basic `evtest` clone, with the added ability to read events from multiple devices concurrently.
