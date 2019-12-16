Haskell evdev library
=====================

http://hackage.haskell.org/package/evdev

This library provides access to the Linux [evdev](https://en.wikipedia.org/wiki/Evdev) interface, for reading input events from devices. It uses [c2hs](https://github.com/haskell/c2hs/wiki/User-Guide)-generated bindings to [libevdev](https://www.freedesktop.org/wiki/Software/libevdev/), which should be available on almost any modern Linux distro.

Modules
-------
- `Evdev` provides the basic functionality for initialising devices, reading events etc.
- `Evdev.Codes` contains datatypes corresponding to the constants in `input-event-codes.h`, such as keys and device properties.
- `Evdev.Stream` provides a higher-level [Streamly](https://hackage.haskell.org/package/streamly)-based interface, for obtaining a stream of events.


Why streamly?
-------------
Compared to other Haskell streaming libraries, I've found `streamly` to have a remarkably easy-to-use API, and the best, simplest support for concurrency. For example, merging concurrent streams of events, from different devices, is trivial.

If you wish to use this library alongside `conduit`, `pipes` etc. then see [here](https://hackage.haskell.org/package/streamly-0.7.0/docs/Streamly-Tutorial.html#g:39) for a guide on interoperation.

Getting started
---------------
Your user will need to be a member of the `input` group in order to read from devices. Try `usermod -a -G input [username]`.

If you wish to make use of concurrency, e.g. to use functions like `allEvents`, be sure to pass the option `-threaded` to GHC, in order to enable the threaded runtime.

The [evdev-examples](https://github.com/georgefst/evdev/tree/master/evdev-examples) folder contains a basic `evtest` clone, with the added ability to read events from multiple devices concurrently.

See [Hackage](http://hackage.haskell.org/package/evdev) for further documentation. Full descriptions for each function, datatype etc. will be uploaded in the very near future.
