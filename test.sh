#!/bin/sh
set -ex
cd riscv-tests/isa/
riscv32-linux-gnu-gcc \
    -march=rv32g \
    -mabi=ilp32 \
    -static \
    -mcmodel=medany \
    -fvisibility=hidden \
    -nostdlib \
    -nostartfiles \
    -DENTROPY=0xf7930f7 \
    -std=gnu99 \
    -O2 \
    -I./../env/v \
    -I./macros/scalar \
    -T./../env/v/link.ld \
    ./../env/v/entry.S \
    ./../env/v/*.c \
    rv32ui/simple.S \
    -o rv32ui-v-simple

