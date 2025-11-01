########################################################################################################################
# TomatoDotNet - linux host
########################################################################################################################

#-----------------------------------------------------------------------------------------------------------------------
# Build Configuration
#-----------------------------------------------------------------------------------------------------------------------

# Nuke built-in rules and variables.
MAKEFLAGS += -rR
.SUFFIXES:

.PHONY: force

CC 				:= clang
AR				:= llvm-ar
LD				:= ld.lld

# Build in debug or release mode
DEBUG			?= 0

# Should we optimize
ifeq ($(DEBUG),1)
OPTIMIZE		?= 0
else
OPTIMIZE		?= 1
endif

#-----------------------------------------------------------------------------------------------------------------------
# Build constants
#-----------------------------------------------------------------------------------------------------------------------

ifeq ($(DEBUG),1)
TDN_BIN_DIR	:= ../../out/debug/bin
else
TDN_BIN_DIR	:= ../../out/release/bin
endif

# The common flags, also passed to TDN
COMMON_CFLAGS			:= -march=x86-64-v3
COMMON_CFLAGS			+= -g

# Check if we need optimization flags
ifeq ($(OPTIMIZE),1)
COMMON_CFLAGS			+= -O3 -flto
else
COMMON_CFLAGS			+= -O0
endif

# Check if we need debugging flags
ifeq ($(DEBUG),1)
COMMON_CFLAGS			+= -fsanitize=undefined
COMMON_CFLAGS 			+= -fno-sanitize=alignment
COMMON_CFLAGS			+= -fsanitize=address
COMMON_CFLAGS 			+= -fstack-protector-all

COMMON_CFLAGS			+= -Wno-unused-but-set-variable -Wno-unused-variable -Wno-unused-function
else
COMMON_CFLAGS 			+= -DNDEBUG
endif

# The flags we need
CFLAGS					:= $(COMMON_CFLAGS)
CFLAGS					+= -Wall -Werror
CFLAGS					+= -Wno-format-invalid-specifier -Wno-unused-label -Wno-unused-function
CFLAGS					+= -fms-extensions -Wno-microsoft-anon-tag
CFLAGS					+= -fno-omit-frame-pointer
CFLAGS					+= -I../../src
CFLAGS					+= -I../../include
CFLAGS					+= -I../../libs/spidir/c-api/include


LDFLAGS					:= $(COMMON_CFLAGS)

# The output directories
OUT_DIR		:= out
BIN_DIR 	:= $(OUT_DIR)/bin
BUILD_DIR	:= $(OUT_DIR)/build

# Get the sources along side all of the objects and dependencies
SRCS 		:= $(shell find . -name '*.c')
OBJS 		:= $(SRCS:%=$(BUILD_DIR)/%.o)
DEPS 		:= $(OBJS:%.o=%.d)

# The default rule
.PHONY: default
default: all

# All the rules
.PHONY: all
all: $(BIN_DIR)/tdn.elf

#-----------------------------------------------------------------------------------------------------------------------
# Rules
#-----------------------------------------------------------------------------------------------------------------------

-include $(DEPS)

$(BUILD_DIR)/%.c.o: %.c
	@echo CC $@
	@mkdir -p $(@D)
	@$(CC) $(CFLAGS) -MMD -MP -c $< -o $@

$(BIN_DIR)/tdn.elf: $(OBJS) $(TDN_BIN_DIR)/libtdn.a
	@echo LD $@
	@mkdir -p $(@D)
	@$(CC) -o $@ \
 		-L$(TDN_BIN_DIR) \
 		-Wl,-whole-archive -l:libtdn.a -Wl,-no-whole-archive \
 		$(OBJS) \
 		$(LDFLAGS) \
 		-lm

$(TDN_BIN_DIR)/libtdn.a: force
	@echo MAKE $@
	@$(MAKE) -C ../.. \
		CC="$(CC)" \
		AR="$(AR)" \
		LD="$(LD)" \
		DEBUG="$(DEBUG)" \
		CFLAGS="$(COMMON_CFLAGS)" \
		CARGO_TARGET_NAME="x86_64-unknown-linux-none" \
		CARGO_FLAGS="--target x86_64-unknown-linux-none -Zbuild-std=core,alloc" \
		RUSTUP_TOOLCHAIN="nightly-2025-05-07"

clean:
	rm -rf $(OUT_DIR)
	$(MAKE) -C ../.. clean
