
/* WIN32 */
#include <windows.h>
#include <mswsock.h>
#include <assert.h>

BOOL c_sendfile(int sock, char* fp) {
    HANDLE fd = CreateFile(fp, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, NULL);
    assert(fd != INVALID_HANDLE_VALUE);
    assert(TransmitFile(sock, fd, 0, 0, NULL, NULL, TF_USE_KERNEL_APC));
    assert(CloseHandle(fd));
    return TRUE;
}
