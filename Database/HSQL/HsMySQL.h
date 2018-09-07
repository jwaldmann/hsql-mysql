#ifndef HsMySQL
#define HsMySQL

#ifdef mingw32_HOST_OS
#include <windows.h>
#endif

#include <mysql.h>

#ifdef CLIENT_MULTI_STATEMENTS
#define MYSQL_DEFAULT_CONNECT_FLAGS  CLIENT_MULTI_STATEMENTS
#else
#define MYSQL_DEFAULT_CONNECT_FLAGS  0
#endif

#endif
