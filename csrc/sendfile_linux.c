
/* LINUX */
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/sendfile.h>
#include <sys/stat.h>

/* return value is an errno code or 0 if there is no error */
int c_sendfile_linux(const int out_fd, const char* in_fp, const long _offset, const long _count) {
    off_t offset = _offset;
    size_t count = _count;
    int in_fd = open(in_fp, O_RDONLY);
    
    if(in_fd == -1) {
        return errno;
    }
    
    if(sendfile(out_fd, in_fd, &offset, count) == -1) {
        return errno;
    }

    if(close(in_fd) == -1) {
        return errno;
    }
    
    /* return 0 to indicate no errors */
    return 0;
}
