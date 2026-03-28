{
  description = "rv32-zig dev shell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    zig-overlay.url = "github:mitchellh/zig-overlay";
  };

  outputs = { self, nixpkgs, flake-utils, zig-overlay }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-linux" ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        zig  = zig-overlay.packages.${system}."0.15.2";

        baremetal = import nixpkgs {
          inherit system;
          crossSystem = {
            config = "riscv32-none-elf";
            libc   = "newlib";
            gcc    = { arch = "rv32ima"; abi = "ilp32"; };
          };
        };

        linux = import nixpkgs {
          inherit system;
          crossSystem = {
            config = "riscv32-unknown-linux-gnu";
            libc   = "glibc";
            gcc    = { arch = "rv32ima"; abi = "ilp32"; };
          };
        };

        packages = {
          baremetal-gcc      = baremetal.buildPackages.gcc;
          baremetal-binutils = baremetal.buildPackages.binutils;
          linux-gcc          = linux.buildPackages.gcc;
          linux-binutils     = linux.buildPackages.binutils;
        };
      in {
        inherit packages;

        devShells.default = pkgs.mkShell {
          packages = builtins.attrValues packages ++ [ zig ];
        };
      }
    );
}
