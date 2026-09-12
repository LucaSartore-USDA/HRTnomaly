#ifndef __HRT_TEB_H__
#define __HRT_TEB_H__

#include <stdint.h>

#ifndef __GNUC__
#define __asm__ asm
#endif

static inline uint8_t* MYCurrentTeb() {
    uint8_t *ptr = NULL;
    #if defined(__i386__)  // 32-bit x86
    __asm__ volatile (
        "movl %%gs:0x30, %0"   // Move from gs:[0x30] into output operand
        : "=r" (ptr)           // Output: any general-purpose register
        :                      // No input operands
        :                      // No clobbers
    );
    #elif defined(__x86_64__) // 64-bit x86_64
    __asm__ volatile (
        "movq %%gs:0x30, %0"   // Move from gs:[0x30] into output operand
        : "=r" (ptr)
        :
        :
    );
    #endif
    return ptr;
}

#endif