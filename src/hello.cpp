#include <iostream>

extern "C" {
    #include "libhello.h"
}

int main() {
    hello();
    return 0;
}
