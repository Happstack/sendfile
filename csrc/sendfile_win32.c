
/* WIN32 */
#include <windows.h>
#include <mswsock.h>
#include <io.h>

BOOL c_TransmitFile(SOCKET out_fd, HANDLE in_hdl, DWORD count) {
    /* http://msdn.microsoft.com/en-us/library/ms740565(VS.85).aspx */
    return TransmitFile(out_fd, in_hdl, count, 0, NULL, NULL, TF_USE_KERNEL_APC);
}

long c_get_osfhandle(int fd) {
    /* http://support.microsoft.com/kb/99173 - MAY BE IMPORTANT */
    /* http://msdn.microsoft.com/en-us/library/ks2530z6.aspx */
    return _get_osfhandle(fd);
}

