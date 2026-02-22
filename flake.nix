{
  description = "RISCV64 Embedded Toolchain Shell";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
  };
  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
    in {
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = [
          pkgs.pkgsCross.riscv64-embedded.buildPackages.gcc
          pkgs.pkgsCross.riscv64-embedded.buildPackages.gdb
          pkgs.gdb
        ];
      };
    };
}
