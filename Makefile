# RISCV_PREFIX = riscv64-elf-
RISCV_PREFIX = riscv64-none-elf-
# RV32UI = $(wildcard riscv-tests/isa/rv32ui/*.S)
TESTS = riscv-tests/isa
RV32UI = $(patsubst $(TESTS)/rv32ui/%.S,%,$(wildcard $(TESTS)/rv32ui/*.S))
RV32UM = $(patsubst $(TESTS)/rv32um/%.S,%,$(wildcard $(TESTS)/rv32um/*.S))

all: riscv-tests-rv32ui riscv-tests-rv32um
.PHONY = all

riscv-tests-rv32ui:
	git submodule update --init --recursive
	XLEN=32 RISCV_PREFIX=$(RISCV_PREFIX) make -C riscv-tests/isa rv32ui
	$(foreach var,$(RV32UI),$(RISCV_PREFIX)objcopy -O binary $(TESTS)/rv32ui-p-$(var) $(TESTS)/rv32ui-p-$(var).bin;)

riscv-tests-rv32um:
	git submodule update --init --recursive
	XLEN=32 RISCV_PREFIX=$(RISCV_PREFIX) make -C riscv-tests/isa rv32um
	$(foreach var,$(RV32UM),$(RISCV_PREFIX)objcopy -O binary $(TESTS)/rv32um-p-$(var) $(TESTS)/rv32um-p-$(var).bin;)
