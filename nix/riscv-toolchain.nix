{
  pkgs, stdenv, lib,
  arch
}:

stdenv.mkDerivation rec {
  pname    = "riscv-${arch}-toolchain";
  version = "2026.03.13";
  src     = pkgs.fetchFromGitHub {
    owner = "riscv-collab";
    repo = "riscv-gnu-toolchain";
    rev = "refs/tags/${version}";
    sha256 = "sha256-LR+cOFze3O0nkxoV+kpeYBkmaL5df4JzLxDuZPMNMuY=";
    fetchSubmodules = true;
    leaveDotGit = true;
  };

  nativeBuildInputs = with pkgs; [
    curl
    perl
    python3
    gawk
    texinfo
    bison
    flex
    gperf
    git
    flock
  ];
  buildInputs = with pkgs; [
    libmpc
    mpfr
    gmp
    expat
  ];

  configureFlags   = [
    "--with-arch=${arch}"
    "--with-abi=ilp32"
  ];
  postConfigure = ''
    # nixpkgs will set those value to bare string "ar", "objdump"...
    # however we are cross-compiling, we must let $CC to determine which bintools to use.
    unset AR AS LD OBJCOPY OBJDUMP
  '';
  hardeningDisable = [ "all" ];
  enableParallelBuilding = true;
  dontPatchELF = true;
  dontStrip = true;

  makeFlags = [
    # Don't auto update source
    "GCC_SRC_GIT="
    "BINUTILS_SRC_GIT="
    "GLIBC_SRC_GIT="
    "GDB_SRC_GIT="
    "NEWLIB_SRC_GIT="

    # Install to nix out dir
    "INSTALL_DIR=${placeholder "out"}"
  ];

  installTargets = "newlib linux";
}
