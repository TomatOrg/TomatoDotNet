#include "mem_tree.h"

#include <stdlib.h>
#include <tomatodotnet/util/c-rbtree.h>

typedef struct mem_tree_node {
    CRBNode node;
    void* object;
    size_t size;
} mem_tree_node_t;

static CRBTree m_mem_tree = C_RBTREE_INIT;

static int mem_tree_node_compare(CRBTree* t, void* k, CRBNode* n) {
    mem_tree_node_t* node = (mem_tree_node_t*)n;
    if (node->object <= k && k < node->object + node->size) {
        return 0;
    }
    return k < node->object ? -1 : 1;
}

void mem_tree_insert(void* object, size_t size) {
    // create the new node
    mem_tree_node_t* n = malloc(sizeof(*n));
    assert(n);
    n->object = object;
    n->size = size;
    c_rbnode_init(&n->node);

    // insert it
    CRBNode* p;
    CRBNode** slot = c_rbtree_find_slot(&m_mem_tree, mem_tree_node_compare, object, &p);
    assert(slot != NULL);
    c_rbtree_add(&m_mem_tree, p, slot, &n->node);
}

void mem_tree_remove(void* object) {
    mem_tree_node_t* node = c_rbtree_find_entry(&m_mem_tree, mem_tree_node_compare, object, mem_tree_node_t, node);
    assert(node != NULL);
    c_rbnode_unlink(&node->node);
    free(node);
}

void* mem_tree_find(void* ptr_in_object) {
    mem_tree_node_t* node = c_rbtree_find_entry(&m_mem_tree, mem_tree_node_compare, ptr_in_object, mem_tree_node_t, node);
    if (node == NULL) {
        return NULL;
    }
    return node->object;
}

void mem_tree_iter_begin(mem_tree_iter_t* iter) {
    iter->iter = c_rbtree_first(&m_mem_tree);
    iter->safe = c_rbnode_next(iter->iter);
}

void* mem_tree_iter_current(mem_tree_iter_t* iter) {
    if (iter->iter == NULL) {
        return NULL;
    }
    return c_rbnode_entry(iter->iter, mem_tree_node_t, node)->object;
}

void mem_tree_iter_next(mem_tree_iter_t* iter) {
    iter->iter = iter->safe;
    iter->safe = c_rbnode_next(iter->safe);
}

void mem_tree_destroy() {
    c_rbtree_init(&m_mem_tree);
}
