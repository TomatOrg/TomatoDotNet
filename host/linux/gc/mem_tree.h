#pragma once

#include <stdbool.h>
#include <stddef.h>

#include "c-rbtree.h"

typedef struct mem_tree_iter {
    CRBNode* iter;
    CRBNode* safe;
} mem_tree_iter_t;

void mem_tree_insert(void* object, size_t size);

void mem_tree_remove(void* object);

void* mem_tree_find(void* ptr_in_object);

void mem_tree_iter_begin(mem_tree_iter_t* iter);

void* mem_tree_iter_current(mem_tree_iter_t* iter);

void mem_tree_iter_next(mem_tree_iter_t* iter);

void mem_tree_destroy();
