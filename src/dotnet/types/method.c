
#include "tinydotnet/except.h"
#include "tinydotnet/types/basic.h"
#include "tinydotnet/types/reflection.h"
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
