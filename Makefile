# RISCV_PREFIX = riscv64-elf-
RISCV_PREFIX = riscv32-none-elf-
# RV32UI = $(wildcard riscv-tests/isa/rv32ui/*.S)
TESTS = riscv-tests/isa
RV32UI = $(patsubst $(TESTS)/rv32ui/%.S,%,$(wildcard $(TESTS)/rv32ui/*.S))
RV32UM = $(patsubst $(TESTS)/rv32um/%.S,%,$(wildcard $(TESTS)/rv32um/*.S))
RV32UA = $(patsubst $(TESTS)/rv32ua/%.S,%,$(wildcard $(TESTS)/rv32ua/*.S))
RV32MI = $(patsubst $(TESTS)/rv32mi/%.S,%,$(wildcard $(TESTS)/rv32mi/*.S))
RV32SI = $(patsubst $(TESTS)/rv32si/%.S,%,$(wildcard $(TESTS)/rv32si/*.S))

all: riscv-tests-rv32ui riscv-tests-rv32um riscv-tests-rv32ua riscv-tests-rv32mi riscv-tests-rv32si
.PHONY = all

riscv-tests-rv32ui:
	git submodule update --init --recursive
	XLEN=32 RISCV_PREFIX=$(RISCV_PREFIX) make -C riscv-tests/isa rv32ui
	$(foreach var,$(RV32UI),$(RISCV_PREFIX)objcopy -O binary $(TESTS)/rv32ui-p-$(var) $(TESTS)/rv32ui-p-$(var).bin;)

riscv-tests-rv32um:
	git submodule update --init --recursive
	XLEN=32 RISCV_PREFIX=$(RISCV_PREFIX) make -C riscv-tests/isa rv32um
	$(foreach var,$(RV32UM),$(RISCV_PREFIX)objcopy -O binary $(TESTS)/rv32um-p-$(var) $(TESTS)/rv32um-p-$(var).bin;)

riscv-tests-rv32ua:
	git submodule update --init --recursive
	XLEN=32 RISCV_PREFIX=$(RISCV_PREFIX) make -C riscv-tests/isa rv32ua
	$(foreach var,$(RV32UA),$(RISCV_PREFIX)objcopy -O binary $(TESTS)/rv32ua-p-$(var) $(TESTS)/rv32ua-p-$(var).bin;)

riscv-tests-rv32mi:
	git submodule update --init --recursive
	XLEN=32 RISCV_PREFIX=$(RISCV_PREFIX) make -C riscv-tests/isa rv32mi
	$(foreach var,$(RV32MI),$(RISCV_PREFIX)objcopy -O binary $(TESTS)/rv32mi-p-$(var) $(TESTS)/rv32mi-p-$(var).bin;)

riscv-tests-rv32si:
	git submodule update --init --recursive
	XLEN=32 RISCV_PREFIX=$(RISCV_PREFIX) make -C riscv-tests/isa rv32si
	$(foreach var,$(RV32SI),$(RISCV_PREFIX)objcopy -O binary $(TESTS)/rv32si-p-$(var) $(TESTS)/rv32si-p-$(var).bin;)

clean:
	XLEN=32 RISCV_PREFIX=$(RISCV_PREFIX) make -C riscv-tests/isa clean
	rm $(TESTS)/rv32*-p-*.bin
