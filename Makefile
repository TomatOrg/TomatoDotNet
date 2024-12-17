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

# The spidir compilation target (given to cargo)
SPIDIR_TARGET	?= x86_64-unknown-none

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
TDN_CFLAGS		:= $(CFLAGS)
TDN_CFLAGS		+= -Wall -Werror
TDN_CFLAGS		+= -std=gnu17
TDN_CFLAGS		+= -g
TDN_CFLAGS		+= -Wno-unused-label
TDN_CFLAGS		+= -Wno-address-of-packed-member
TDN_CFLAGS		+= -Wno-unused-function -Wno-format-invalid-specifier
TDN_CFLAGS		+= -fms-extensions -Wno-microsoft-anon-tag
TDN_CFLAGS		+= -Iinclude -Isrc -Ilibs/spidir/c-api/include

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
LIBSPIDIR	:= libs/spidir/target/$(SPIDIR_TARGET)/debug/libspidir.a
else
LIBSPIDIR	:= libs/spidir/target/$(SPIDIR_TARGET)/release/libspidir.a
endif

# The dlls that we need for the runtime
DLLS 		:= $(BIN_DIR)/System.Private.CoreLib.dll

# The default rule
.PHONY: default
default: all

# All the rules
.PHONY: all
all: $(BIN_DIR)/libtdn.a $(DLLS)

#-----------------------------------------------------------------------------------------------------------------------
# Rules
#-----------------------------------------------------------------------------------------------------------------------

-include $(DEPS)

$(BUILD_DIR)/%.c.o: %.c
	@echo CC $@
	@mkdir -p $(@D)
	@$(CC) $(TDN_CFLAGS) -MMD -c $< -o $@

$(BIN_DIR)/libtdn.a: $(OBJS)
	@echo AR $@
	@mkdir -p $(@D)
	@$(AR) rc $@ $^

clean:
	rm -rf out
	rm -rf libs/spidir/target
	rm -rf TdnCoreLib/*/bin
	rm -rf TdnCoreLib/*/obj
	$(MAKE) -C host/linux clean

#-----------------------------------------------------------------------------------------------------------------------
# C# rules
#-----------------------------------------------------------------------------------------------------------------------

# Choose the build configuration
ifeq ($(DEBUG),1)
DOTNET_CONFIG 	:= Debug
else
DOTNET_CONFIG 	:= Release
endif

# TODO: turn into a macro

$(BIN_DIR)/System.Private.CoreLib.dll: TdnCoreLib/System.Private.CoreLib/bin/$(DOTNET_CONFIG)/net8.0/System.Private.CoreLib.dll
	@mkdir -p $(@D)
	cp $^ $@

TdnCoreLib/System.Private.CoreLib/bin/$(DOTNET_CONFIG)/net8.0/System.Private.CoreLib.dll: force
	cd TdnCoreLib/System.Private.CoreLib && dotnet build --configuration $(DOTNET_CONFIG)

#-----------------------------------------------------------------------------------------------------------------------
# Spidir rules
#-----------------------------------------------------------------------------------------------------------------------

# We are going to compile the entire libspidir.a into a single object file for easier
# linking of the tdn library
$(BUILD_DIR)/spidir.o: $(LIBSPIDIR)
	@echo CC $@
	@mkdir -p $(@D)
	@$(LD) -r --whole-archive -o $@ $^

libs/spidir/target/$(SPIDIR_TARGET)/release/libspidir.a: force
	cd libs/spidir/c-api && cargo build --release -p c-api --target $(SPIDIR_TARGET)

libs/spidir/target/$(SPIDIR_TARGET)/debug/libspidir.a: force
	cd libs/spidir/c-api && cargo build -p c-api --target $(SPIDIR_TARGET)
