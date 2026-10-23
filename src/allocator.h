#ifndef HRTNOMALY_ALLOCATOR_H

    #define HRTNOMALY_ALLOCATOR_H

    #include "myomp.h"

    #if __VOPENMP

        #define mi_malloc(size) omp_alloc(size, omp_null_allocator)
        #define mi_calloc(n, size) omp_calloc(n, size, omp_null_allocator)
        #define mi_free(ptr) omp_free(ptr, omp_null_allocator)
        /* #define mi_realloc(ptr, size) omp_realloc(ptr, size, omp_null_allocator, omp_null_allocator) */
    
    #else

        #include <stdlib.h>

        #define mi_malloc malloc
        #define mi_calloc calloc
        #define mi_free free
        /* #define mi_realloc realloc */

    #endif

#endif
