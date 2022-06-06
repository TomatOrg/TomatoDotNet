#include <mimalloc.h>
#include <mimalloc-types.h>
#include <mimalloc-internal.h>
#include "mimalloc_region.h"
#include <dotnet/gc/heap.h>

extern mem_region_t mi_regions[MI_REGION_MAX];
extern _Atomic(size_t) mi_regions_count;

void heap_reclaim() {}

System_Object heap_find_fast(void *ptr) {
    mi_segment_t* segment = _mi_ptr_segment(ptr);
    mi_page_t* page = _mi_segment_page_of(segment, ptr);
    uintptr_t page_start = (uintptr_t)_mi_segment_page_start(segment, page, page->xblock_size, NULL, NULL);
    uintptr_t diff = ((uintptr_t)ptr - page_start) % page->xblock_size;
    uintptr_t start = (uintptr_t)ptr - diff;
    return (void*)start;
}

System_Object heap_find(uintptr_t p) {
    if (!p) return NULL;
    void* ptr = (void*)p;
    if (!mi_is_in_heap_region(ptr)) return NULL;
    mi_segment_t* segment = _mi_ptr_segment(ptr);
    if (_mi_ptr_cookie(segment) != segment->cookie) { return NULL; }
    mi_page_t* page = _mi_segment_page_of(segment, ptr);
    uintptr_t page_start = (uintptr_t)_mi_segment_page_start(segment, page, page->xblock_size, NULL, NULL);
    uintptr_t diff = ((uintptr_t)ptr - page_start) % page->xblock_size;
    uintptr_t start = (uintptr_t)ptr - diff;
    return (void*)start;
}
    
void heap_iterate_objects(object_callback_t callback) {
    size_t count = mi_regions_count;
    for (size_t i = 0; i < count; i++) { // iterate through regions
        uint8_t* start = mi_regions[i].start;
        if (start != NULL) {
            size_t bitmap = mi_regions[i].in_use;
            for (size_t j = 0; j < MI_BITMAP_FIELD_BITS; j++) { // iterate through segments in region
                if (bitmap & (1UL << j)) { // segment is in use
                    mi_segment_t *seg = (void*)(start + j * MI_SEGMENT_SIZE);
                    ASSERT(seg->page_kind == MI_PAGE_SMALL);
                    for (size_t k = 0; k < MI_SMALL_PAGES_PER_SEGMENT; k++) { // iterate through pages: page_small case
                        mi_page_t *p = &seg->pages[k];
                        if (!p->xblock_size) continue;
                        uintptr_t obj = (uintptr_t)_mi_page_start(seg, p, NULL);
                        for (size_t l = 0; l < p->capacity; l++) { 
                            System_Object o = (void*)obj;
                            callback(o);
                            obj += p->xblock_size;
                        }
                    }
                }
            }
        }
    }
}

// TODO: actually only iterate through dirty objects
void heap_iterate_dirty_objects(object_callback_t callback) {
    if (callback) heap_iterate_objects(callback);
}

System_Object heap_alloc(size_t size, int color) {
    System_Object o = mi_malloc(size);
    memset(o, 0, size);
    o->color = color;
    return o;
}

void heap_free(System_Object o) {
    mi_free(o);
}
