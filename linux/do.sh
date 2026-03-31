#!/bin/sh

set -ex

cd riscv-pk
mkdir -p build
cd build
../configure \
    --host=riscv32-unknown-elf \
    CC=riscv32-none-elf-gcc \
    CXX=riscv32-none-elf-g++ \
    AR=riscv32-none-elf-ar \
    RANLIB=riscv32-none-elf-ranlib \
    READELF=riscv32-none-elf-readelf \
    OBJCOPY=riscv32-none-elf-objcopy
make
riscv32-none-elf-objcopy -O binary bbl bbl.bin
