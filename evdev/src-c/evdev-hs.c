#include <libevdev-1.0/libevdev/libevdev.h>
#include <stdio.h>
#include <unistd.h>

void libevdev_hs_close(struct libevdev *dev) {
    int fd = libevdev_get_fd(dev);
    libevdev_free(dev);
    close(fd);
}
