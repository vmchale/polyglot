%{
void compiler_version(void) {
    prtinf("compiled with gcc version: %d.%d.%d\n", __GNUC__, __GNUC_MINOR__, __GNUC_PATCHLEVEL__);
}

#include <features.h>

void libc_version(void) {
    printf("glibc version: %d.%d\n", __GLIBC__, __GLIBC_MINOR__);
}
%}

fn compiler_version() : void =
  "#ext"

fn libc_version() : void =
  "ext#"
