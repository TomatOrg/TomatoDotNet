#include "jit_builtin.h"

#include <dotnet/types.h>
#include <tomatodotnet/types/type.h>
#include <util/except.h>

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// System.Object
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static void emit_object_get_type(spidir_builder_handle_t builder) {
    // get the object
    spidir_value_t arg0 = spidir_builder_build_param_ref(builder, 0);

    // deref the vtable pointer and convert to pointer
    spidir_value_t vtable = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_4, SPIDIR_TYPE_I64, arg0);
    vtable = spidir_builder_build_inttoptr(builder, vtable);

    // load the type from the vtable
    spidir_value_t type = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR, vtable);
    STATIC_ASSERT(offsetof(ObjectVTable, Type) == 0);

    // return it
    spidir_builder_build_return(builder, type);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// System.Runtime.CompilerServices.Unsafe
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static void emit_unsafe_add(spidir_builder_handle_t builder, RuntimeMethodBase base) {
    RuntimeTypeInfo T = base->GenericArguments->Elements[0];
    bool need_iext = base->Parameters->Elements[1]->ParameterType == tInt32;

    spidir_value_t source = spidir_builder_build_param_ref(builder, 0);
    spidir_value_t element_offset = spidir_builder_build_param_ref(builder, 1);

    // check if we need to extend the
    // first parameter to 32bit
    if (need_iext) {
        element_offset = spidir_builder_build_iext(builder, element_offset);
        element_offset = spidir_builder_build_sfill(builder, 32, element_offset);
    }

    // if this is larger than zero we need to multiply
    if (T->StackSize > 1) {
        element_offset = spidir_builder_build_imul(builder, element_offset,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, T->StackSize));
    }

    // and now get the ptroff
    spidir_value_t ptr = spidir_builder_build_ptroff(builder, source, element_offset);

    // and return it
    spidir_builder_build_return(builder, ptr);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Generic emit code
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

void jit_emit_builtin(spidir_builder_handle_t handle, void* _ctx) {
    tdn_err_t err = TDN_NO_ERROR;
    jit_builtin_context_t* ctx = _ctx;
    RuntimeMethodBase method = ctx->method;
    RuntimeTypeInfo type = method->DeclaringType;

    // prepare the main block
    spidir_block_t block = spidir_builder_create_block(handle);
    spidir_builder_set_block(handle, block);
    spidir_builder_set_entry_block(handle, block);

    // and now check it
    if (type == tObject) {
        CHECK(tdn_compare_string_to_cstr(method->Name, "GetType"));
        emit_object_get_type(handle);

    } else if (type == tUnsafe) {
        if (tdn_compare_string_to_cstr(method->Name, "Add")) {
            emit_unsafe_add(handle, method);
        } else {
            CHECK_FAIL();
        }

    } else {
        CHECK_FAIL();
    }

cleanup:
    ctx->err = err;
}
