{
  description = "rv32-zig dev shell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    zig-overlay.url = "github:mitchellh/zig-overlay";
    zigtools-zls.url         = "github:zigtools/zls/0.15.1";
  };

  outputs = { self, nixpkgs, flake-utils, zig-overlay, zigtools-zls }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-linux" ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        zig  = zig-overlay.packages.${system}."0.15.2";
        zls = zigtools-zls.packages.${system}.zls.overrideAttrs (_: {
          nativeBuildInputs = [ zig ];
        });

        baremetal = import nixpkgs {
          inherit system;
          crossSystem = {
            config = "riscv32-none-elf";
            libc   = "newlib";
            gcc    = { arch = "rv32ima_zicsr_zifencei"; abi = "ilp32"; };
          };
        };

        linux = import nixpkgs {
          inherit system;
          crossSystem = {
            config = "riscv32-unknown-linux-gnu";
            libc   = "glibc";
            gcc    = { arch = "rv32ima_zicsr_zifencei"; abi = "ilp32"; };
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
          hardeningDisable = [ "relro" "bindnow" ];
          packages = builtins.attrValues packages ++ [ zig zls ];
        };
      }
    );
}
