{
  description = "rv32-zig dev shell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-linux" ] (system:
      let
        pkgs = import nixpkgs { inherit system; };
        riscv32 = pkgs.pkgsCross.riscv32.riscv32-unknown-elf;
      in {
        devShells.default = pkgs.mkShell {
          hardeningDisable = [ "relro" "bindnow" ];
          # NOTE: These toolchains are compiled for rv32imac.  To restrict the ISA call gcc with `-march=rv32ima -mabi=ilp32`
          packages = [
            # Baremetal toolchain with newlib
            pkgs.pkgsCross.riscv32-embedded.gcc
            pkgs.pkgsCross.riscv32-embedded.binutils
            # pkgs.pkgsCross.riscv32-embedded.newlib
            # Linux toolchain with glibc
            # TODO
          ];
        };
      }
    );
}
