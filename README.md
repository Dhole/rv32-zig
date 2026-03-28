# Notes

Build the riscv tests:
```
nix develop
make
```

Run tests:
```
zig build riscv-tests -Dtest-filter=rv32ui
```
