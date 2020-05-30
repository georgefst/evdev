Evdev
=====

This library provides access to the Linux [evdev](https://en.wikipedia.org/wiki/Evdev) interface, for reading input events from devices. It uses [c2hs](https://github.com/haskell/c2hs/wiki/User-Guide)-generated bindings to [libevdev](https://www.freedesktop.org/wiki/Software/libevdev/), which should be available on almost any modern Linux distro.

It aims to expose the full set of functionality exposed by `libevdev`, while providing stronger types, and a higher level of abstraction - no worrying about memory management, ordering of operations etc.

Getting started
---------------
Your user will need to be a member of the `input` group in order to read from devices. Try `usermod -a -G input [username]`.

If you wish to make use of concurrency, e.g. to use functions like `allEvents`, be sure to pass the option `-threaded` to GHC, in order to enable the threaded runtime.

The [evdev-examples](https://github.com/georgefst/evdev/tree/master/evdev-examples) folder contains a basic `evtest` clone, with the added ability to read events from multiple devices concurrently.

Streamly
--------

For a higher-level, more functional API based on event streams, see [evdev-streamly](http://hackage.haskell.org/package/evdev-streamly).
