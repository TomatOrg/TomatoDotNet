
#include <dotnet/assembly.h>
#include <dotnet/type.h>
#include <dotnet/method.h>
#include <string.h>
#include <dotnet/jit.h>

int main() {
    err_t err = NO_ERROR;

    assembly_t* assembly = NULL;
    CHECK_AND_RETHROW(load_assembly("/home/tomato/projects/tinydotnet/Corelib/Corelib/bin/Release/net5.0/Corelib.dll", &assembly));

    type_t* type = assembly_get_type_by_name(assembly, "Corelib", "Program");
    CHECK(type != NULL);

    method_t* method = NULL;
    for (int i = 0; i < type->methods_count; i++) {
        if (strcmp(type->methods[i].name, "Main") == 0) {
            method = &type->methods[i];
        }
    }
    CHECK(method != NULL);

    jit_instance_t instance = { 0 };
    CHECK_AND_RETHROW(jit_prepare_assembly(&instance, assembly));

//    MIR_context_t ctx = MIR_init();
//
//    MIR_module_t mod = MIR_new_module(ctx, "lol");
//    MIR_type_t restype = MIR_T_I64;
//
//    MIR_item_t p_func1 = MIR_new_proto(ctx, "p_func1", 1, &restype, 0);
//
//    MIR_item_t f_func1 = MIR_new_forward(ctx, "func1");
//    MIR_item_t f_func12 = MIR_new_forward(ctx, "func1");
//    MIR_item_t f_func13 = MIR_new_forward(ctx, "func1");
//    MIR_item_t func2 = MIR_new_func(ctx, "func2", 1, &restype, 0);
//    MIR_reg_t reg = MIR_new_func_reg(ctx, func2->u.func, MIR_T_I64, "retval");
//    MIR_append_insn(ctx, func2,
//                    MIR_new_call_insn(ctx, 3,
//                                      MIR_new_ref_op(ctx, p_func1),
//                                      MIR_new_ref_op(ctx, f_func1),
//                                      MIR_new_reg_op(ctx, reg)));
//    MIR_append_insn(ctx, func2,
//                    MIR_new_ret_insn(ctx, 1, MIR_new_reg_op(ctx, reg)));
//    MIR_finish_func(ctx);
//
//    MIR_item_t func1 = MIR_new_func(ctx, "func1", 1, &restype, 0);
//    MIR_append_insn(ctx, func1,
//                    MIR_new_ret_insn(ctx, 1, MIR_new_int_op(ctx, 1)));
//    MIR_finish_func(ctx);
//
//    MIR_finish_module(ctx);
//
//    MIR_output(ctx, stdout);
//
//    MIR_gen_init(ctx, 0);
//    MIR_gen_set_optimize_level(ctx, 0, 4);
//    MIR_load_module(ctx, mod);
//    MIR_link(ctx, MIR_set_gen_interface, NULL);
//
//    void* addr = MIR_gen(ctx, 0, func2);
//    _MIR_dump_code("lol", 0, func2->u.func->machine_code, 256);
//
//    MIR_finish(ctx);

cleanup:
    return IS_ERROR(err) ? EXIT_FAILURE : EXIT_SUCCESS;
}
