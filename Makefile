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
CARGO_TARGET_NAME	?= x86_64-unknown-linux-none

# Additional flags to pass to cargo
CARGO_FLAGS			?= --target x86_64-unknown-linux-none -Zbuild-std=core,alloc

# Override rust toolchain used to build spidir
RUSTUP_TOOLCHAIN	?= nightly-2025-05-07

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
TDN_CFLAGS		+= -fno-omit-frame-pointer
TDN_CFLAGS		+= -Wno-unused-label
TDN_CFLAGS		+= -Wno-address-of-packed-member
TDN_CFLAGS		+= -Wno-format-invalid-specifier
TDN_CFLAGS		+= -fms-extensions -Wno-microsoft-anon-tag
TDN_CFLAGS		+= -Iinclude -Isrc
TDN_CFLAGS		+= -Ilibs/spidir/c-api/include
TDN_CFLAGS		+= -Ilibs/icu4x/ffi/capi/bindings/c
TDN_CFLAGS		+= -Ilibs/utf8-utf16-converter/converter/include
TDN_CFLAGS		+= $(CFLAGS)

ifeq ($(DEBUG),1)
TDN_CFLAGS		+= -D__TDN_DEBUG__
endif

# Get the sources along side all of the objects and dependencies
SRCS 		:= $(shell find src -name '*.c')
SRCS		+= libs/utf8-utf16-converter/converter/src/converter.c

OBJS 		:= $(SRCS:%=$(BUILD_DIR)/%.o)
DEPS 		:= $(OBJS:%.o=%.d)

# Add the spidir object
OBJS 		+= $(BUILD_DIR)/tdn-rust-libs.o

# Choose which of the rust libs we want to use
ifeq ($(SPIDIR_DEBUG),1)
RUST_LIBS := out/cargo-target/$(CARGO_TARGET_NAME)/debug/libtdn_rust_libs.a
else
RUST_LIBS := out/cargo-target/$(CARGO_TARGET_NAME)/release/libtdn_rust_libs.a
endif

# The default rule
.PHONY: default
default: all

# All the rules
.PHONY: all
all: $(BIN_DIR)/libtdn.a

.PHONY: run
run:
	make -C host/linux
	cd TdnCoreLib/Tests && dotnet build --configuration Debug
	./host/linux/out/bin/tdn.elf ./TdnCoreLib/Tests/bin/Debug/net8.0/Tests.dll --search-path ./TdnCoreLib/Tests/bin/Debug/net8.0


# Quick test
.PHONY: test
test:
	make -C host/linux
	python3 ./tests/test.py


# Quick test
.PHONY: test
test-verifier:
	make -C host/linux
	python3 ./tests/test.py ilverify

#-----------------------------------------------------------------------------------------------------------------------
# Rules
#-----------------------------------------------------------------------------------------------------------------------

-include $(DEPS)

$(BUILD_DIR)/%.c.o: %.c
	@echo CC $@
	@mkdir -p $(@D)
	@$(CC) $(TDN_CFLAGS) -MMD -MP -c $(abspath $<) -o $@

$(BIN_DIR)/libtdn.a: $(OBJS)
	@echo AR $@
	@mkdir -p $(@D)
	@$(AR) rc $@ $^

clean:
	rm -rf out

#-----------------------------------------------------------------------------------------------------------------------
# Rust rules
#-----------------------------------------------------------------------------------------------------------------------

CARGO_CMD 	:= cargo
ifneq ($(RUSTUP_TOOLCHAIN),)
CARGO_CMD 	+= +$(RUSTUP_TOOLCHAIN)
endif
CARGO_CMD	+= rustc
CARGO_CMD 	+= --manifest-path libs/tdn-rust-libs/Cargo.toml
ifneq ($(SPIDIR_DEBUG),1)
CARGO_CMD	+= --release
endif
CARGO_CMD 	+= $(CARGO_FLAGS)
CARGO_CMD 	+= --target-dir out/cargo-target
CARGO_CMD 	+= --
CARGO_CMD	+= -C force-frame-pointers=yes
CARGO_CMD	+= $(RUSTC_FLAGS)

# We are going to compile all the rusts into a single object file for easier
# linking of the tdn library
$(BUILD_DIR)/tdn-rust-libs.o: $(RUST_LIBS)
	@echo LD $@
	@mkdir -p $(@D)
	@$(LD) -r --whole-archive -o $@ $^

$(RUST_LIBS): force
	$(CARGO_CMD)
