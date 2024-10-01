// main.c
#include <stdio.h>
#include <stdlib.h>
#include <dlfcn.h>
#include "libhello.h"

int main() {
    void *handle = dlopen("lib/libhello.so", RTLD_LAZY);
    if (!handle) {
        fprintf(stderr, "\e[31mError: %s\n\e[m", dlerror());
        exit(EXIT_FAILURE);
    }

    dlerror();

    void (*hello)() = (void (*)()) dlsym(handle, "hello");
    char *error = dlerror();
    if (error != NULL) {
        fprintf(stderr, "\e[31mError: %s\n\e[m", error);
        dlclose(handle);
        exit(EXIT_FAILURE);
    }

    hello();

    dlclose(handle);

    return 0;
}
