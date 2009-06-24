
/* WIN32 */
#include <windows.h>
#include <mswsock.h>

/* return value is system error code defined here:
 * http://msdn.microsoft.com/en-us/library/ms681381(VS.85).aspx
 */
int c_sendfile_win32(int out_fd, char* in_fp) {
    HANDLE in_hdl =
        CreateFile(
          in_fp,
          GENERIC_READ,
          FILE_SHARE_READ,
          NULL,
          OPEN_EXISTING,
          FILE_FLAG_SEQUENTIAL_SCAN,
          NULL
        );
    if(in_hdl == INVALID_HANDLE_VALUE) {
        return GetLastError();
    }
    
    if(! TransmitFile(out_fd, in_hdl, 0, 0, NULL, NULL, TF_USE_KERNEL_APC)) {
        return WSAGetLastError();
    }
    
    if(! CloseHandle(in_hdl)) {
        return GetLastError();
    }
    
    /* if no errors, return 0 to indicate no errors */
    return 0;
}
