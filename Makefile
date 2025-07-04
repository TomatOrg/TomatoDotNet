########################################################################################################################
# TomatoDotNet
########################################################################################################################

#-----------------------------------------------------------------------------------------------------------------------
# Build Configuration
#-----------------------------------------------------------------------------------------------------------------------

# Nuke built-in rules and variables.
MAKEFLAGS += -rR
.SUFFIXES:

.PHONY: force

# Use clang by default
CC				:= clang
AR				:= llvm-ar
LD				:= ld.lld

# Should we compile in debug
DEBUG			?= 0

# Should we compile in debug or not
SPIDIR_DEBUG	?= $(DEBUG)

# Additional flags to pass to cargo
SPIDIR_CARGO_FLAGS 			?=

# Override rust toolchain used to build spidir
SPIDIR_RUSTUP_TOOLCHAIN		?=

# Flags to pass to the rustc compiler
SPIDIR_RUSTC_FLAGS			?=

# The cflags
CFLAGS			?=

#-----------------------------------------------------------------------------------------------------------------------
# Build constants
#-----------------------------------------------------------------------------------------------------------------------

# The output directories
ifeq ($(DEBUG),1)
OUT_DIR			:= out/debug
else
OUT_DIR			:= out/release
endif

BIN_DIR 		:= $(OUT_DIR)/bin
BUILD_DIR		:= $(OUT_DIR)/build

# Add some flags that we require to work
TDN_CFLAGS		:= -Wall -Werror
TDN_CFLAGS		+= -std=gnu17
TDN_CFLAGS		+= -g
TDN_CFLAGS		+= -Wno-unused-label
TDN_CFLAGS		+= -Wno-address-of-packed-member
TDN_CFLAGS		+= -Wno-format-invalid-specifier
TDN_CFLAGS		+= -fms-extensions -Wno-microsoft-anon-tag
TDN_CFLAGS		+= -Iinclude -Isrc -Ilibs/spidir/c-api/include
TDN_CFLAGS		+= $(CFLAGS)

ifeq ($(DEBUG),1)
TDN_CFLAGS		+= -D__TDN_DEBUG__
endif

# Get the sources along side all of the objects and dependencies
SRCS 		:= $(shell find src -name '*.c')
OBJS 		:= $(SRCS:%=$(BUILD_DIR)/%.o)
DEPS 		:= $(OBJS:%.o=%.d)

# Add the spidir object
OBJS 		+= $(BUILD_DIR)/spidir.o

# Choose which of the spidirs we want to use
ifeq ($(SPIDIR_DEBUG),1)
LIBSPIDIR	:= out/cargo-target/target/debug/libspidir.a
else
LIBSPIDIR	:= out/cargo-target/target/release/libspidir.a
endif

# The default rule
.PHONY: default
default: all

# All the rules
.PHONY: all
all: $(BIN_DIR)/libtdn.a

#-----------------------------------------------------------------------------------------------------------------------
# Rules
#-----------------------------------------------------------------------------------------------------------------------

-include $(DEPS)

$(BUILD_DIR)/%.c.o: %.c
	@echo CC $@
	@mkdir -p $(@D)
	@$(CC) $(TDN_CFLAGS) -MMD -c $(abspath $<) -o $@

$(BIN_DIR)/libtdn.a: $(OBJS)
	@echo AR $@
	@mkdir -p $(@D)
	@$(AR) rc $@ $^

clean:
	rm -rf out

#-----------------------------------------------------------------------------------------------------------------------
# Spidir rules
#-----------------------------------------------------------------------------------------------------------------------

CARGO_CMD 	:= cargo
ifneq ($(SPIDIR_RUSTUP_TOOLCHAIN),)
CARGO_CMD	+= +$(SPIDIR_RUSTUP_TOOLCHAIN)
endif
CARGO_CMD	+= rustc
CARGO_CMD	+= --manifest-path libs/spidir/c-api/Cargo.toml
ifneq ($(SPIDIR_DEBUG),1)
CARGO_CMD	+= --release
endif
CARGO_CMD	+= -p c-api
CARGO_CMD	+= $(SPIDIR_CARGO_FLAGS)
CARGO_CMD 	+= --target-dir out/cargo-target
CARGO_CMD 	+= --
CARGO_CMD	+= -C force-frame-pointers=yes
CARGO_CMD	+= $(SPIDIR_RUSTC_FLAGS)


# We are going to compile the entire libspidir.a into a single object file for easier
# linking of the tdn library
$(BUILD_DIR)/spidir.o: $(LIBSPIDIR)
	@echo CC $@
	@mkdir -p $(@D)
	@$(LD) -r --whole-archive -o $@ $^

$(LIBSPIDIR): force
	$(CARGO_CMD)
