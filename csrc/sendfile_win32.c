
/* WIN32 */
#include <windows.h>
#include <mswsock.h>

/* return value is 0 if there is no error otherwise it is a system error code defined here:
 * http://msdn.microsoft.com/en-us/library/ms681381(VS.85).aspx
 */
int c_sendfile_win32(const int out_fd, const char* in_fp, const long _offset, const long _count) {
    LONG offset = _offset;
    DWORD count = _count;
    HANDLE in_hdl = CreateFile(
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
    
    if(SetFilePointer(in_hdl, offset, NULL, FILE_BEGIN) == INVALID_SET_FILE_POINTER) {
        return GetLastError();
    }
    
    if(! TransmitFile(out_fd, in_hdl, count, 0, NULL, NULL, TF_USE_KERNEL_APC)) {
        return WSAGetLastError();
    }
    
    if(! CloseHandle(in_hdl)) {
        return GetLastError();
    }
    
    /* return 0 to indicate no errors */
    return 0;
}
