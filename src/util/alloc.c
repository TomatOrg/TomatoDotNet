#include "alloc.h"

#include <tomatodotnet/host.h>

void* tdn_mallocz(size_t size) {
    return tdn_host_mallocz(size, 8);
}
