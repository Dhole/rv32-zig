{
  description = "RV32 dev environment";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs";

  outputs = { self, nixpkgs }:
  let
    system = "x86_64-linux";
    pkgs = import nixpkgs { inherit system; };
    toolchain = pkgs.callPackage ./nix/riscv-toolchain.nix {
      arch = "rv32ima";
    };
  in {
    packages.${system}.default = toolchain;
    devShells.${system}.default = pkgs.mkShell {
      buildInputs = [
        toolchain
      ];
    };
  };
}
