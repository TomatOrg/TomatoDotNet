#include "trace.h"

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

void trace_hex(const void* _data, size_t size) {
    const uint8_t* data = _data;
    char ascii[17] = { 0 };

    printf("[*] ");
    for (int i = 0; i < size; i++) {
        printf("%02x ", data[i]);

        if (data[i] >= ' ' && data[i] <= '~') {
            ascii[i % 16] = data[i];
        } else {
            ascii[i % 16] = '.';
        }

        if ((i + 1) % 8 == 0 || i + 1 == size) {
            printf(" ");
            if ((i + 1) % 16 == 0) {
                printf("|  %s \n", ascii);
                if (i + 1 != size) {
                    printf("[*] ");
                }
            } else if (i + 1 == size) {
                ascii[(i + 1) % 16] = '\0';
                if ((i + 1) % 16 <= 8) {
                    printf(" ");
                }
                for (int j = (i + 1) % 16; j < 16; ++j) {
                    printf("   ");
                }
                printf("|  %s \n", ascii);
            }
        }
    }
}

void _putchar(char character) {
    putc_unlocked(character, stdout);
}
