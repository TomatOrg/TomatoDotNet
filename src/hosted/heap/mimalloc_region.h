#pragma once

#include <mimalloc.h>
#include <mimalloc-internal.h>
#include <mimalloc-atomic.h>
#include <mimalloc/src/bitmap.h>

#include <string.h>  // memset

// Constants
#if (MI_INTPTR_SIZE==8)
#define MI_HEAP_REGION_MAX_SIZE    (256 * MI_GiB)  // 64KiB for the region map 
#elif (MI_INTPTR_SIZE==4)
#define MI_HEAP_REGION_MAX_SIZE    (3 * MI_GiB)    // ~ KiB for the region map
#else
#error "define the maximum heap space allowed for regions on this platform"
#endif

#define MI_SEGMENT_ALIGN          MI_SEGMENT_SIZE

#define MI_REGION_MAX_BLOCKS      MI_BITMAP_FIELD_BITS
#define MI_REGION_SIZE            (MI_SEGMENT_SIZE * MI_BITMAP_FIELD_BITS)    // 256MiB  (64MiB on 32 bits)
#define MI_REGION_MAX             (MI_HEAP_REGION_MAX_SIZE / MI_REGION_SIZE)  // 1024  (48 on 32 bits)
#define MI_REGION_MAX_OBJ_BLOCKS  (MI_REGION_MAX_BLOCKS/4)                    // 64MiB
#define MI_REGION_MAX_OBJ_SIZE    (MI_REGION_MAX_OBJ_BLOCKS*MI_SEGMENT_SIZE)  

// Region info 
typedef union mi_region_info_u {
  size_t value;      
  struct {
    bool  valid;        // initialized?
    bool  is_large:1;   // allocated in fixed large/huge OS pages
    bool  is_pinned:1;  // pinned memory cannot be decommitted
    short numa_node;    // the associated NUMA node (where -1 means no associated node)
  } x;
} mi_region_info_t;


// A region owns a chunk of REGION_SIZE (256MiB) (virtual) memory with
// a bit map with one bit per MI_SEGMENT_SIZE (4MiB) block.
typedef struct mem_region_s {
  _Atomic(size_t)           info;        // mi_region_info_t.value
  _Atomic(void*)            start;       // start of the memory area 
  mi_bitmap_field_t         in_use;      // bit per in-use block
  mi_bitmap_field_t         dirty;       // track if non-zero per block
  mi_bitmap_field_t         commit;      // track if committed per block
  mi_bitmap_field_t         reset;       // track if reset per block
  _Atomic(size_t)           arena_memid; // if allocated from a (huge page) arena
  _Atomic(size_t)           padding;     // round to 8 fields (needs to be atomic for msvc, see issue #508)
} mem_region_t;
