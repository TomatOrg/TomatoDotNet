#pragma once

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

typedef struct list_entry {
    struct list_entry* next;
    struct list_entry* prev;
} list_entry_t;

typedef list_entry_t list_t;

#define CR(ptr, type, member) \
    ((type*)((uint8_t*)(ptr) - offsetof(type, member)))

#define LIST_ENTRY(ptr, type, member) \
	CR(ptr, type, member)

#define LIST_FIRST_ENTRY(ptr, type, member) \
	LIST_ENTRY((ptr)->next, type, member)

#define LIST_NEXT_ENTRY(pos, member) \
    LIST_ENTRY((pos)->member.next, typeof(*(pos)), member)

#define LIST_ENTRY_IS_HEAD(pos, head, member) \
    (&pos->member == (head))

#define LIST_FOR_EACH_ENTRY_SAFE(pos, n, head, member) \
	for (pos = LIST_FIRST_ENTRY(head, typeof(*pos), member), \
		n = LIST_NEXT_ENTRY(pos, member); \
	     !LIST_ENTRY_IS_HEAD(pos, head, member); \
	     pos = n, n = LIST_NEXT_ENTRY(n, member))

static inline bool list_is_empty(list_t* head) {
    return head->next == head;
}

static inline void list_del(list_entry_t* entry) {
    list_entry_t* prev = entry->prev;
    list_entry_t* next = entry->next;

    next->prev = prev;
    prev->next = next;

    entry->next = NULL;
    entry->prev = NULL;
}
