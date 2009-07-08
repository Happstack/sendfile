
/* LINUX */
#include <stdlib.h>
#include <sys/sendfile.h>

ssize_t c_sendfile_linux(const int out_fd, const int in_fd, const size_t count) {
    return sendfile(out_fd, in_fd, NULL, count);
}

