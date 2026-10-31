/* A copy-on-write copy of one file where the filesystem has them (APFS:
   clonefile(2)); -1 elsewhere, and the caller copies the bytes instead. */
#if defined(__APPLE__)
#define _DARWIN_C_SOURCE
#include <sys/clonefile.h>
int olifant_clone(const char *src, const char *dst) { return clonefile(src, dst, 0); }
#else
int olifant_clone(const char *src, const char *dst) { (void)src; (void)dst; return -1; }
#endif
