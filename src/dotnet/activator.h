#pragma once

#include "types.h"

#include "util/except.h"

err_t activator_create_instance(System_Type type, System_Object* args, int argsCount, System_Object* created);

System_Exception activator_create_exception(System_Type type);
