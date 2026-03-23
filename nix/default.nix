let
  pkgs = import <nixpkgs> {};
in
pkgs.callPackage ./riscv-toolchain.nix {
  arch = "rv32ima";
}
