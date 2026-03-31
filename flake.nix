{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }: let
    pkgs = import nixpkgs { system = "x86_64-linux"; };

    # Bare-metal: riscv32, no OS
    pkgsBare = import nixpkgs {
      system = "x86_64-linux";
      crossSystem = {
        config   = "riscv32-none-elf";
        libc     = "newlib";
        gcc = {
          arch = "rv32ima";
          abi  = "ilp32";
        };
      };
    };

    # GNU/Linux target
    pkgsLinux = import nixpkgs {
      system = "x86_64-linux";
      crossSystem = {
        config = "riscv32-unknown-linux-gnu";
        gcc = {
          arch = "rv32ima";
          abi  = "ilp32";
        };
      };
    };

  in {
    devShells.x86_64-linux = {

    default = pkgs.mkShell {
      name = "rv32ima";
      hardeningDisable = [ "relro" "bindnow" ];
      nativeBuildInputs = [
        # Baremetal
        pkgsBare.buildPackages.gcc
        pkgsBare.buildPackages.binutils
        pkgsBare.buildPackages.gdb

        # Linux
        pkgsLinux.buildPackages.gcc
        pkgsLinux.buildPackages.binutils
        pkgsLinux.buildPackages.gdb
      ];
    };
    };
  };
}
