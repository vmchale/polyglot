// from here: https://stackoverflow.com/a/22330309
#ifdef __APPLE__
int get_nprocs() {
    int count;
    size_t count_len = sizeof(count);
    sysctlbyname("hw.logicalcpu", &count, &count_len, NULL, 0);
    return count;
}
#elif defined(__x86_64__)
extern int get_nprocs() {
  int t;

    asm volatile (
        "cpuid\n"
        "mov %0, %%eax\n"
        "shr $0x10, %0\n"
        "and $0xf, %0\n"
        "dec %0"
        : "=r" (t)
    );

  return t;
}
#endif
