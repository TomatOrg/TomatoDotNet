
#include <spidir/log.h>
#include <tomatodotnet/tdn.h>

static tdn_config_t m_tdn_config = {
    .jit_verify_trace = false,
    .jit_emit_trace = false,
    .jit_elf_dump = false,
    .jit_spidir_dump = false,
    .jit_spidir_log_level = SPIDIR_LOG_LEVEL_WARN,
    .jit_inline = true,
    .jit_optimize = true,
};

tdn_config_t* tdn_get_config(void) {
    return &m_tdn_config;
}
