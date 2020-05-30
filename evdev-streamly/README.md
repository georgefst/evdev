Evdev + Streamly
================

This library provides provides a high level [Streamly](https://hackage.haskell.org/package/streamly)-based interface to [evdev](http://hackage.haskell.org/package/evdev), for working with streams of input events.

It doesn't re-export anything, so you will almost certainly also need to depend directly on both of those packages.

Why streamly?
-------------
Compared to other Haskell streaming libraries, I've found `streamly` to have a remarkably easy-to-use API, and the best, simplest support for concurrency. For example, merging concurrent streams of events, from different devices, is trivial.

If you wish to use this library alongside `conduit`, `pipes` etc. then see [here](https://hackage.haskell.org/package/streamly-0.7.0/docs/Streamly-Tutorial.html#g:39) for a guide on interoperation.
