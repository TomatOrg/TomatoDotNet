#include <string.h>
#include "mir_utils.h"

MIR_item_t mir_get_func(MIR_context_t ctx, const char* name) {
    DLIST(MIR_module_t)* modules = MIR_get_module_list(ctx);
    for (MIR_module_t module = DLIST_HEAD (MIR_module_t, *modules); module != NULL; module = DLIST_NEXT (MIR_module_t, module)) {
        for (MIR_item_t item = DLIST_HEAD (MIR_item_t, module->items); item != NULL; item = DLIST_NEXT (MIR_item_t, item)) {
            if (item->item_type == MIR_func_item && strcmp(item->u.func->name, name) == 0) {
                return item;
            }
        }
    }
    return NULL;
}

MIR_item_t mir_get_forward(MIR_context_t ctx, const char* name) {
    DLIST(MIR_module_t)* modules = MIR_get_module_list(ctx);
    for (MIR_module_t module = DLIST_HEAD (MIR_module_t, *modules); module != NULL; module = DLIST_NEXT (MIR_module_t, module)) {
        for (MIR_item_t item = DLIST_HEAD (MIR_item_t, module->items); item != NULL; item = DLIST_NEXT (MIR_item_t, item)) {
            if (item->item_type == MIR_forward_item && strcmp(item->u.forward_id, name) == 0) {
                return item;
            }
        }
    }
    return NULL;
}

MIR_item_t mir_get_proto(MIR_context_t ctx, const char* name) {
    DLIST(MIR_module_t)* modules = MIR_get_module_list(ctx);
    for (MIR_module_t module = DLIST_HEAD (MIR_module_t, *modules); module != NULL; module = DLIST_NEXT (MIR_module_t, module)) {
        for (MIR_item_t item = DLIST_HEAD (MIR_item_t, module->items); item != NULL; item = DLIST_NEXT (MIR_item_t, item)) {
            if (item->item_type == MIR_proto_item && strcmp(item->u.proto->name, name) == 0) {
                return item;
            }
        }
    }
    return NULL;
}
