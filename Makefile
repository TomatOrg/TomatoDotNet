########################################################################################################################
# Build constants
########################################################################################################################

CC 			:= ccache clang
LD			:= clang

# Build in debug or release mode
DEBUG		:= 1

OUT_DIR		:= out
BIN_DIR		:= $(OUT_DIR)/bin
BUILD_DIR	:= $(OUT_DIR)/build

#-----------------------------------------------------------------------------------------------------------------------
# General configurations
#-----------------------------------------------------------------------------------------------------------------------

CFLAGS 		:= -target x86_64-pc-linux-elf
#CFLAGS		+= -Werror -std=gnu17
CFLAGS 		+= -Wno-unused-label
CFLAGS 		+= -Wno-address-of-packed-member
CFLAGS		+= -Wno-unused-function -Wno-format-invalid-specifier

ifeq ($(DEBUG),1)
	CFLAGS	+= -Og -g
#	CFLAGS	+= -fsanitize=undefined,address
#	CFLAGS 	+= -fno-sanitize=alignment
	CFLAGS 	+= -fstack-protector-all
else
	CFLAGS	+= -O3 -g -flto
	CFLAGS 	+= -DNDEBUG
endif

CFLAGS		+= -fms-extensions -Wno-microsoft-anon-tag
CFLAGS 		+= -march=x86-64-v3
CFLAGS 		+= -Iinclude -Isrc -Ilibs/spidir/c-api/include

SRCS 		:= $(shell find src -name '*.c')
SRCS 		+= $(shell find host/linux -name '*.c')

LDFLAGS		:= $(CFLAGS)

########################################################################################################################
# Targets
########################################################################################################################

#
# The DLLs required, we will build them as part of TDN
#
DLLS 	:= TdnCoreLib/System.Private.CoreLib/bin/Debug/net8.0/System.Private.CoreLib.dll
DLLS	+= TdnCoreLib/Tests/bin/Debug/net8.0/Tests.dll

all: $(BIN_DIR)/tdn.elf $(DLLS)

clean:
	rm -rf out

OBJS := $(SRCS:%=$(BUILD_DIR)/%.o)
DEPS := $(OBJS:%.o=%.d)
BINS ?=
-include $(DEPS)

# add spidir as a dep

ifeq ($(DEBUG),1)
	OBJS += libs/spidir/target/x86_64-unknown-none/debug/libspidir.a
else
	OBJS += libs/spidir/target/x86_64-unknown-none/release/libspidir.a
endif

$(BIN_DIR)/tdn.elf: $(OBJS)
	@echo LD $@
	@mkdir -p $(@D)
	@$(LD) -o $@ $^ $(LDFLAGS)

$(BUILD_DIR)/%.c.o: %.c
	@echo CC $@
	@mkdir -p $(@D)
	@$(CC) -Wall $(CFLAGS) -MMD -c $< -o $@

#-----------------------------------------------------------------------------------------------------------------------
# C# binaries
#-----------------------------------------------------------------------------------------------------------------------

TdnCoreLib/System.Private.CoreLib/bin/Debug/net8.0/System.Private.CoreLib.dll: force
	cd TdnCoreLib/System.Private.CoreLib && dotnet build

TdnCoreLib/Tests/bin/Debug/net8.0/Tests.dll: force
	cd TdnCoreLib/Tests && dotnet build

#-----------------------------------------------------------------------------------------------------------------------
# Spidir lib
#-----------------------------------------------------------------------------------------------------------------------

.PHONY: force

# spidir targets
libs/spidir/target/x86_64-unknown-none/release/libspidir.a: force
	cd libs/spidir/c-api && cargo build --release -p c-api --target x86_64-unknown-none

libs/spidir/target/x86_64-unknown-none/debug/libspidir.a: force
	cd libs/spidir/c-api && cargo build -p c-api --target x86_64-unknown-none
