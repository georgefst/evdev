Haskell evdev library
=====================

See Hackage for documentation on [the core library](http://hackage.haskell.org/package/evdev) and [the streamly add-on](http://hackage.haskell.org/package/evdev-streamly).

The [evdev-examples](https://github.com/georgefst/evdev/tree/master/evdev-examples) folder contains a basic `evtest` clone, with the added ability to read events from multiple devices concurrently.

Still to come
-------------
Haddock documentation for the non-stream interface. I intend to redesign the interface in some minor ways first. Until then, everything maps quite directly to the C library, while taking care of the nitty-gritty low-level stuff.

There are many more methods in the C library that I'd like to expose. So far the emphasis has very much been on squeezing out the functionality I personally needed.

Some of the streaming functions print exceptions straight to `stderr`. Again, this was convenient at the time, but it would be cleaner to allow specifying a `Writer`, or passing callbacks.
