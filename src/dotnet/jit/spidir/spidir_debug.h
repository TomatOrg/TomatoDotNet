#pragma once

#if 1

#define spidir_builder_set_block(builder, block) \
    ({ \
        spidir_block_t __block = block; \
        TRACE("spidir_builder_set_block(block=%d)", __block.id); \
        spidir_builder_set_block(builder, __block); \
    })

#define spidir_builder_build_param_ref(...) \
    ({ \
        TRACE("spidir_builder_build_param_ref"); \
        spidir_builder_build_param_ref(__VA_ARGS__); \
    })

#define spidir_builder_build_call(...) \
    ({ \
        TRACE("spidir_builder_build_call"); \
        spidir_builder_build_call(__VA_ARGS__); \
    })

#define spidir_builder_build_return(...) \
    ({ \
        TRACE("spidir_builder_build_return"); \
        spidir_builder_build_return(__VA_ARGS__); \
    })

#define spidir_builder_build_branch(builder, dest) \
    ({ \
        spidir_block_t __dest = dest; \
        TRACE("spidir_builder_build_branch(dest=%d)", __dest.id); \
        spidir_builder_build_branch(builder, __dest); \
    })

#define spidir_builder_build_brcond(builder, cond, true_dest, false_dest) \
    ({ \
        spidir_block_t __true_dest = true_dest; \
        spidir_block_t __false_dest = false_dest; \
        TRACE("spidir_builder_build_brcond(true_dest=%d, false_dest=%d)", __true_dest.id, __false_dest.id); \
        spidir_builder_build_brcond(builder, cond, true_dest, false_dest); \
    })

#define spidir_builder_build_phi(...) \
    ({ \
        TRACE("spidir_builder_build_phi"); \
        spidir_builder_build_phi(__VA_ARGS__); \
    })

#define spidir_builder_build_iconst(...) \
    ({ \
        TRACE("spidir_builder_build_iconst"); \
        spidir_builder_build_iconst(__VA_ARGS__); \
    })

#define spidir_builder_build_iadd(...) \
    ({ \
        TRACE("spidir_builder_build_iadd"); \
        spidir_builder_build_iadd(__VA_ARGS__); \
    })

#define spidir_builder_build_isub(...) \
    ({ \
        TRACE("spidir_builder_build_isub"); \
        spidir_builder_build_isub(__VA_ARGS__); \
    })

#define spidir_builder_build_and(...) \
    ({ \
        TRACE("spidir_builder_build_and"); \
        spidir_builder_build_and(__VA_ARGS__); \
    })

#define spidir_builder_build_or(...) \
    ({ \
        TRACE("spidir_builder_build_or"); \
        spidir_builder_build_or(__VA_ARGS__); \
    })

#define spidir_builder_build_xor(...) \
    ({ \
        TRACE("spidir_builder_build_xor"); \
        spidir_builder_build_xor(__VA_ARGS__); \
    })

#define spidir_builder_build_shl(...) \
    ({ \
        TRACE("spidir_builder_build_shl"); \
        spidir_builder_build_shl(__VA_ARGS__); \
    })

#define spidir_builder_build_lshr(...) \
    ({ \
        TRACE("spidir_builder_build_lshr"); \
        spidir_builder_build_lshr(__VA_ARGS__); \
    })

#define spidir_builder_build_ashr(...) \
    ({ \
        TRACE("spidir_builder_build_ashr"); \
        spidir_builder_build_ashr(__VA_ARGS__); \
    })

#define spidir_builder_build_imul(...) \
    ({ \
        TRACE("spidir_builder_build_imul"); \
        spidir_builder_build_imul(__VA_ARGS__); \
    })

#define spidir_builder_build_sdiv(...) \
    ({ \
        TRACE("spidir_builder_build_sdiv"); \
        spidir_builder_build_sdiv(__VA_ARGS__); \
    })

#define spidir_builder_build_udiv(...) \
    ({ \
        TRACE("spidir_builder_build_udiv"); \
        spidir_builder_build_udiv(__VA_ARGS__); \
    })

#define spidir_builder_build_icmp(...) \
    ({ \
        TRACE("spidir_builder_build_icmp"); \
        spidir_builder_build_icmp(__VA_ARGS__); \
    })

#define spidir_builder_build_ptroff(...) \
    ({ \
        TRACE("spidir_builder_build_ptroff"); \
        spidir_builder_build_ptroff(__VA_ARGS__); \
    })

#define spidir_builder_build_load(...) \
    ({ \
        TRACE("spidir_builder_build_load"); \
        spidir_builder_build_load(__VA_ARGS__); \
    })

#define spidir_builder_build_store(...) \
    ({ \
        TRACE("spidir_builder_build_store"); \
        spidir_builder_build_store(__VA_ARGS__); \
    })

#define spidir_builder_build_stackslot(...) \
    ({ \
        TRACE("spidir_builder_build_stackslot"); \
        spidir_builder_build_stackslot(__VA_ARGS__); \
    })

#endif
