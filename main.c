#include <stdio.h>
#include <windows.h>
#include <mswsock.h>
#include <assert.h>

SOCKET createListenerSocket() {
    SOCKET sock = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    assert(sock != INVALID_SOCKET);
    
    SOCKADDR_IN service;
    service.sin_family = AF_INET;
    service.sin_addr.s_addr = inet_addr("127.0.0.1");
    service.sin_port = htons(8000);
    
    assert(0 == bind(sock, (SOCKADDR*) &service, sizeof(service)));
    //bind(sock, (SOCKADDR*) &service, sizeof(service));
    //printf("%d", WSAGetLastError());
    assert(0 == listen(sock, SOMAXCONN));
    return sock;
}

SOCKET acceptClientSocket(SOCKET listenSock) {
    SOCKET clientSock = accept(listenSock, NULL, NULL);
    assert(clientSock != INVALID_SOCKET);
    return clientSock;
}

void startup() {
    /* Start WinSock 2.2 */
    WSADATA wsaData;
    assert(0 == WSAStartup( MAKEWORD(2, 2), &wsaData));
}

void cleanup() {
    /* Cleanup WinSock */
    WSACleanup();
}

int main() {
    startup();
    SOCKET listenSock = createListenerSocket();
    SOCKET clientSock = acceptClientSocket(listenSock);
    
    HANDLE fd = CreateFile("main.c", GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING,
      FILE_FLAG_OVERLAPPED | FILE_FLAG_SEQUENTIAL_SCAN, NULL);
    assert(fd != INVALID_HANDLE_VALUE);
    
    send(clientSock, "hello, world!\n", 14, 0);
    
    TransmitFile(clientSock, fd, 0, 0, NULL, NULL, TF_USE_KERNEL_APC);
    
    closesocket(clientSock);
    cleanup();
}
