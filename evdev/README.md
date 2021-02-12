Evdev
=====

This library provides access to the Linux [evdev](https://en.wikipedia.org/wiki/Evdev) interface, for reading input events from devices. It uses [c2hs](https://github.com/haskell/c2hs/wiki/User-Guide)-generated bindings to [libevdev](https://www.freedesktop.org/wiki/Software/libevdev/), which should be available on almost any modern Linux distro.

It aims to expose the full set of functionality exposed by `libevdev`, while providing stronger types, and a higher level of abstraction - no worrying about memory management, ordering of operations etc.

For a higher-level, more functional API based on event streams, see [evdev-streamly](http://hackage.haskell.org/package/evdev-streamly).

This package uses `ByteString`s (a.k.a `RawFilePath`s) ubiquitously. You may want to substitute use of packages like `filepath`, `directory`, `unix` and `process` with `ByteString`-friendly alternatives:
- [rawfilepath](https://hackage.haskell.org/package/rawfilepath)
- [filepath-bytestring](https://hackage.haskell.org/package/filepath-bytestring)

# Permissions

Your user will need to be a member of the `input` group in order to read from devices. Try `usermod -a -G input [username]`.

To create virtual devices (i.e. to use the `Evdev.UInput` module) you will need permission to write to `/dev/uinput`. This can usually be achieved by creating a group specially for uinput permissions:
```
sudo groupadd uinput
sudo usermod -a -G uinput $USER
echo 'KERNEL=="uinput", GROUP="uinput", MODE:="0660", OPTIONS+="static_node=uinput"' | sudo tee -a /etc/udev/rules
```
Log out and back in for this to take effect.
<!--TODO mention udevadm,modprobe, as in #14 -->
