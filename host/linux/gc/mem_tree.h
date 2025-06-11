#pragma once

#include <stddef.h>

typedef struct mem_tree_iter {
    void* ctx;
} mem_tree_iter_t;

void mem_tree_insert(void* object, size_t size);

void* mem_tree_find(void* ptr);

void* mem_tree_iter_first(mem_tree_iter_t* iter);

void* mem_tree_iter_next(mem_tree_iter_t* iter);

void mem_tree_destroy();
