
#include "tomatodotnet/except.h"
#include "tomatodotnet/types/basic.h"
#include "tomatodotnet/types/reflection.h"
#include "util/except.h"

tdn_err_t tdn_make_generic_method(
    RuntimeMethodInfo type,
    RuntimeTypeInfo_Array arguments,
    RuntimeMethodInfo* instance
) {
    tdn_err_t err = TDN_NO_ERROR;

    // TODO: implement generic method creation
    CHECK_FAIL();

cleanup:
    return err;
}
