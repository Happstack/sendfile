
/* LINUX */
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/sendfile.h>
#include <sys/stat.h>

/* return value is system error code */
int c_sendfile_linux(int out_fd, char* in_fp) {
    int in_fd;
    struct stat in_stat;

    in_fd = open(in_fp, O_RDONLY);
    if(in_fd == -1) {
        return errno;
    }

    if(fstat(in_fd, &in_stat) == -1) {
        return errno;
    }

    if(sendfile(out_fd, in_fd, NULL, in_stat.st_size) == -1) {
        return errno;
    }

    if(close(in_fd) == -1) {
        return errno;
    }
    
    /* if no errors, return 0 to indicate no errors */
    return 0;
}

