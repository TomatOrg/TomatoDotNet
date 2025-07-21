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
CC					:= clang
AR					:= llvm-ar
LD					:= ld.lld

# Should we compile in debug
DEBUG				?= 0

# Should we compile in debug or not
SPIDIR_DEBUG		?= $(DEBUG)

# The name of the target, this will not actually put it
# as the target, you need to do it yourself with flags
CARGO_TARGET_NAME 	?=

# Additional flags to pass to cargo
CARGO_FLAGS			?=

# Override rust toolchain used to build spidir
RUSTUP_TOOLCHAIN	?=

# Flags to pass to the rustc compiler
RUSTC_FLAGS			?=

# The cflags
CFLAGS				?=

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
TDN_CFLAGS		+= -Iinclude -Isrc
TDN_CFLAGS		+= -Ilibs/spidir/c-api/include
TDN_CFLAGS		+= -Ilibs/icu4x/ffi/capi/bindings/c
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
OBJS 		+= $(BUILD_DIR)/icu4x.o

# Choose which of the spidirs we want to use
ifeq ($(SPIDIR_DEBUG),1)
LIBSPIDIR	:= out/cargo-target/$(CARGO_TARGET_NAME)/debug/libspidir.a
else
LIBSPIDIR	:= out/cargo-target/$(CARGO_TARGET_NAME)/release/libspidir.a
endif

LIBICU4X	:= out/cargo-target/$(CARGO_TARGET_NAME)/release/libtdn_icu4x.a

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

SPIDIR_CARGO_CMD 	:= cargo
ifneq ($(RUSTUP_TOOLCHAIN),)
SPIDIR_CARGO_CMD	+= +$(RUSTUP_TOOLCHAIN)
endif
SPIDIR_CARGO_CMD	+= rustc
SPIDIR_CARGO_CMD	+= --manifest-path libs/spidir/c-api/Cargo.toml
ifneq ($(SPIDIR_DEBUG),1)
SPIDIR_CARGO_CMD	+= --release
endif
SPIDIR_CARGO_CMD	+= -p c-api
SPIDIR_CARGO_CMD	+= $(CARGO_FLAGS)
SPIDIR_CARGO_CMD 	+= --target-dir out/cargo-target
SPIDIR_CARGO_CMD 	+= --
SPIDIR_CARGO_CMD	+= -C force-frame-pointers=yes
SPIDIR_CARGO_CMD	+= $(RUSTC_FLAGS)

# We are going to compile the entire libspidir.a into a single object file for easier
# linking of the tdn library
$(BUILD_DIR)/spidir.o: $(LIBSPIDIR)
	@echo CC $@
	@mkdir -p $(@D)
	@$(LD) -r --whole-archive -o $@ $^

$(LIBSPIDIR): force
	$(SPIDIR_CARGO_CMD)

#-----------------------------------------------------------------------------------------------------------------------
# icu4x rules
#-----------------------------------------------------------------------------------------------------------------------

ICU4X_CARGO_CMD 	:= cargo
ifneq ($(RUSTUP_TOOLCHAIN),)
ICU4X_CARGO_CMD	+= +$(RUSTUP_TOOLCHAIN)
endif
ICU4X_CARGO_CMD	+= rustc
ICU4X_CARGO_CMD	+= --manifest-path libs/tdn-icu4x/Cargo.toml
ICU4X_CARGO_CMD	+= --release
ICU4X_CARGO_CMD	+= $(CARGO_FLAGS)
ICU4X_CARGO_CMD += --target-dir out/cargo-target
ICU4X_CARGO_CMD += --crate-type=staticlib
ICU4X_CARGO_CMD += --
ICU4X_CARGO_CMD	+= -C force-frame-pointers=yes
ICU4X_CARGO_CMD	+= $(RUSTC_FLAGS)

# We are going to compile the entire libicu4x.a into a single object file for easier
# linking of the tdn library
$(BUILD_DIR)/icu4x.o: $(LIBICU4X)
	@echo CC $@
	@mkdir -p $(@D)
	@$(LD) -r --whole-archive -o $@ $^

$(LIBICU4X): force
	$(ICU4X_CARGO_CMD)
