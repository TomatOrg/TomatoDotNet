#include "mem_tree.h"

#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef enum rb_clor {
    RED,
    BLACK,
} rb_clor_t;

typedef struct rb_node {
    struct rb_node* left;
    struct rb_node* right;
    struct rb_node* parent;
    uintptr_t start;
    size_t length;
    rb_clor_t color;
} rb_node_t;

static rb_node_t* m_mem_tree_root = NULL;

static rb_node_t* mem_tree_search_le(uintptr_t x) {
    rb_node_t* cur = m_mem_tree_root;
    rb_node_t* last_le = NULL;
    while (cur != NULL) {
        if (x < cur->start) {
            cur = cur->left;
        } else {
            last_le = cur;
            cur = cur->right;
        }
    }
    return last_le;
}

static void rotate_left(rb_node_t* x) {
    rb_node_t* y = x->right;
    x->right = y->left;
    if (y->left) y->left->parent = x;
    y->parent = x->parent;
    if (!x->parent) m_mem_tree_root = y;
    else if (x == x->parent->left) x->parent->left = y;
    else x->parent->right = y;
    y->left  = x;
    x->parent = y;
}

static void rotate_right(rb_node_t* x) {
    rb_node_t* y = x->left;
    x->left = y->right;
    if (y->right) y->right->parent = x;
    y->parent = x->parent;
    if (!x->parent) m_mem_tree_root = y;
    else if (x == x->parent->right) x->parent->right = y;
    else x->parent->left  = y;
    y->right = x;
    x->parent = y;
}

static void rb_insert_fix(rb_node_t* z) {
    while (z->parent && z->parent->color == RED) {
        rb_node_t* gp = z->parent->parent;
        if (z->parent == gp->left) {
            rb_node_t* y = gp->right;
            if (y && y->color == RED) {
                z->parent->color = y->color = BLACK;
                gp->color = RED;
                z = gp;
            } else {
                if (z == z->parent->right) {
                    z = z->parent;
                    rotate_left(z);
                }
                z->parent->color = BLACK;
                gp->color        = RED;
                rotate_right(gp);
            }
        } else {
            rb_node_t* y = gp->left;
            if (y && y->color == RED) {
                z->parent->color = y->color = BLACK;
                gp->color = RED;
                z = gp;
            } else {
                if (z == z->parent->left) {
                    z = z->parent;
                    rotate_right(z);
                }
                z->parent->color = BLACK;
                gp->color = RED;
                rotate_left(gp);
            }
        }
    }
    m_mem_tree_root->color = BLACK;
}

void mem_tree_insert(void* object, size_t size) {
    uintptr_t start = (uintptr_t)object;
    uintptr_t end = (uintptr_t)object + size;

    rb_node_t* pred = mem_tree_search_le(start);
    assert(pred == NULL || start >= pred->start + pred->length); // overlaps left

    rb_node_t* succ = pred != NULL ? pred->right : m_mem_tree_root;
    while (succ && succ->left) succ = succ->left;
    assert(succ == NULL || succ->start >= end); // overlaps right

    rb_node_t* n = calloc(1, sizeof(*n));
    assert(n != NULL);
    n->start = start;
    n->length = size;
    n->color = RED;

    rb_node_t** link = &m_mem_tree_root;
    rb_node_t* parent = NULL;
    while (*link) {
        parent = *link;
        link = (start < parent->start) ? &parent->left : &parent->right;
    }
    *link = n;
    n->parent = parent;
    rb_insert_fix(n);
}

void* mem_tree_find(void* ptr) {
    uintptr_t x = (uintptr_t)ptr;
    rb_node_t* n  = mem_tree_search_le(x);
    if (n && x - n->start < n->length) return n;
    return NULL;
}

static rb_node_t* minimum(rb_node_t* n) {
    while (n && n->left) n = n->left;
    return n;
}

void* mem_tree_iter_first(mem_tree_iter_t* iter) {
    rb_node_t* n = minimum(m_mem_tree_root);

    iter->ctx = n;
    return (void*)n->start;
}

void* mem_tree_iter_next(mem_tree_iter_t* iter) {
    rb_node_t* n = iter->ctx;
    if (!n) return NULL;
    if (n->right) {
        n = minimum(n->right);

        iter->ctx = n;
        return (void*)n->start;
    }

    rb_node_t* p = n->parent;
    while (p && n == p->right) {
        n = p;
        p = p->parent;
    }

    if (p == NULL) {
        return NULL;
    }

    iter->ctx = p;
    return (void*)p->start;
}

static void mem_tree_destroy_subtree(rb_node_t* n) {
    if (n == NULL) return;
    mem_tree_destroy_subtree(n->left);
    mem_tree_destroy_subtree(n->right);
    free(n);
}

void mem_tree_destroy() {
    mem_tree_destroy_subtree(m_mem_tree_root);
}
