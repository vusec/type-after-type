#define _GNU_SOURCE

#include <dlfcn.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

#include "sanitizer_common.h"

static const char *SanitizerToolName = "typesafestack";

static uptr GetPageSize() {
#if SANITIZER_LINUX && (defined(__x86_64__) || defined(__i386__))
  return EXEC_PAGESIZE;
#else
  return sysconf(_SC_PAGESIZE);  // EXEC_PAGESIZE may not be trustworthy.
#endif
}

static uptr GetPageSizeCached() {
  static uptr PageSize;
  if (!PageSize)
    PageSize = GetPageSize();
  return PageSize;
}

static uptr internal_mmap(void *addr, size_t length, int prot, int flags,
  int fd, off_t offset) {
  return (uptr) mmap(addr, length, prot, flags, fd, offset);
}

static int internal_munmap(void *addr, size_t length) {
  return munmap(addr, length);
}

static int internal_mprotect(void *addr, size_t len, int prot) {
  return mprotect(addr, len, prot);
}

static void ReportMmapFailureAndDie(uptr size, const char *mem_type, int reserrno) {
  fprintf(stderr, "mmap failed; size=%lu, mem_type=%s: %s\n",
	(long) size, mem_type, strerror(reserrno));
  exit(-1);
}

static void IncreaseTotalMmap(uptr size) {
}

static void DecreaseTotalMmap(uptr size) {
}

void *MmapOrDie(uptr size, const char *mem_type) {
  size = RoundUpTo(size, GetPageSizeCached());
  uptr res = internal_mmap(nullptr, size,
                           PROT_READ | PROT_WRITE,
                           MAP_PRIVATE | MAP_ANON, -1, 0);
  int reserrno = errno;
  if (res == (uptr) MAP_FAILED)
    ReportMmapFailureAndDie(size, mem_type, reserrno);
  IncreaseTotalMmap(size);
  return (void *)res;
}

void UnmapOrDie(void *addr, uptr size) {
  if (!addr || !size) return;
  uptr res = internal_munmap(addr, size);
  if (res != 0) {
    fprintf(stderr, "ERROR: %s failed to deallocate 0x%zx (%zd) bytes at address %p\n",
           SanitizerToolName, size, size, addr);
    CHECK("unable to unmap" && 0);
  }
  DecreaseTotalMmap(size);
}

bool MprotectNoAccess(uptr addr, uptr size) {
  return 0 == internal_mprotect((void*)addr, size, PROT_NONE);
}

bool GetRealFunctionAddress(const char *func_name, uptr *func_addr,
    uptr real, uptr wrapper) {
  *func_addr = (uptr)dlsym(RTLD_NEXT, func_name);
  return real == wrapper;
}

#if !defined(__ANDROID__)  // android does not have dlvsym
void *GetFuncAddrVer(const char *func_name, const char *ver) {
  return dlvsym(RTLD_NEXT, func_name, ver);
}
#endif  // !defined(__ANDROID__)


