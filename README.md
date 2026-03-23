# Notes

Build the riscv tests:
```
# My machine has 32 threads and ram can get scarce by running 32 jobs in
# parallel, so I set the `--core` flags to that make only uses 8 jobs.
TMPDIR=/var/tmp nix develop --cores 8
make
```

Run tests:
```
zig test src/tests.zig --test-filter rv32ui
```

My `/tmp` is tmpfs with 16GB, which is not enough to store all the build artifacts, so I set `TMPDIR` to a disk path:

```
sudo systemctl edit nix-daemon.service
```

Then write this and save:
```
[Service]
Environment="TMPDIR=/var/tmp"
```
