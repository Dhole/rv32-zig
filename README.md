# Notes

Build the riscv tests:
```
nix develop
make
```

Run tests:
```
zig test src/tests.zig --test-filter rv32ui
```
