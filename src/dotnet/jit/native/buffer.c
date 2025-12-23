#include "native.h"

#include "dotnet/types.h"
#include "tomatodotnet/tdn.h"
#include "util/except.h"

static String String_FastAllocateString(int len) {
    String new_str = tdn_gc_new(tString, sizeof(struct String) + (len + 1) * 2);
    ASSERT(new_str != NULL);
    new_str->Length = len;
    return new_str;
}
NATIVE_FUNC_FOR(System, String, FastAllocateString, INT32);

static void Buffer_BulkMoveWithWriteBarrier(void* _dest, void* _src, size_t len) {
    // TODO: something faster, this is done to ensure that there can't be data races
    ASSERT((len % 8) == 0);
    uint64_t* dest = (uint64_t*)_dest;
    uint64_t* src = (uint64_t*)_src;
    for (size_t i = 0; i < len / 4; i++) {
        dest[i] = src[i];
    }
}
NATIVE_FUNC_FOR(System, Buffer, BulkMoveWithWriteBarrier, BY_REF, BY_REF, NATIVE_INT);

static void Buffer_Memmove(void* dest, void* src, size_t len) {
    __builtin_memmove(dest, src, len);
}
NATIVE_FUNC_FOR(System, Buffer, Memmove, BY_REF, BY_REF, NATIVE_INT);

static void SpanHelpers_ClearWithoutReferences(void* b, size_t byteLength) {
    __builtin_memset(b, 0, byteLength);
}
NATIVE_FUNC_FOR(System, SpanHelpers, ClearWithoutReferences, BY_REF, NATIVE_INT);

static void SpanHelpers_ClearWithReferences(void* b, size_t pointerSizeLength) {
    uint64_t* dest = (uint64_t*)b;
    for (size_t i = 0; i < pointerSizeLength; i++) {
        dest[i] = 0;
    }
}
NATIVE_FUNC_FOR(System, SpanHelpers, ClearWithReferences, BY_REF, NATIVE_INT);

static void RuntimeHelpers_InitializeArray(Array array, RuntimeFieldInfo* field_handle) {
    RuntimeFieldInfo field = *field_handle;
    ASSERT(field->Attributes.Static);
    ASSERT(field->HasRVA);
    ASSERT(field->JitFieldPtr != NULL);

    // ensure that the size of the array is the same as the data size in the struct
    RuntimeTypeInfo array_type = array->VTable->Type->ElementType;
    ASSERT(array_type->IsUnmanaged);
    ASSERT(field->FieldType->StackSize == array->Length * array_type->StackSize);

    // and now copy it
    size_t data_offset = tdn_get_array_elements_offset(array_type);
    memcpy((void*)array + data_offset, field->JitFieldPtr, field->FieldType->StackSize);
}
NATIVE_FUNC_FOR(System.Runtime.CompilerServices, RuntimeHelpers, InitializeArray, OBJ_REF, VALUE_TYPE);

static void* RuntimeHelpers_GetSpanDataFrom(RuntimeFieldInfo* field_handle, RuntimeTypeInfo type, int* count) {
    RuntimeFieldInfo field = *field_handle;
    ASSERT(field->Attributes.Static);
    ASSERT(field->HasRVA);
    ASSERT(field->JitFieldPtr != NULL);

    // ensure that the size of the array is the same as the data size in the struct
    ASSERT(type->IsUnmanaged);
    *count = field->FieldType->StackSize /  type->StackSize;
    return field->JitFieldPtr;
}
NATIVE_FUNC_FOR(System.Runtime.CompilerServices, RuntimeHelpers, GetSpanDataFrom, VALUE_TYPE, OBJ_REF, BY_REF);
