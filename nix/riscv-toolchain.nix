{
  pkgs, stdenv, lib,
  arch
}:

pkgs.stdenvNoCC.mkDerivation rec {
  pname    = "riscv-${arch}-toolchain";
  version = "2026.03.13";
  src     = pkgs.fetchFromGitHub {
    owner = "riscv-collab";
    repo = "riscv-gnu-toolchain";
    rev = "refs/tags/${version}";
    sha256 = "sha256-LR+cOFze3O0nkxoV+kpeYBkmaL5df4JzLxDuZPMNMuY=";
    fetchSubmodules = true;
  };

  nativeBuildInputs = with pkgs; [
    gcc binutils
    which
    automake
    autoconf
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
    zlib
    libmpc
    mpfr
    gmp
    expat
  ];

  bits = if lib.hasPrefix "rv32" arch then "32" else "64";
  abi  = if bits == "32" then "ilp32" else "lp64";
  configureFlags = [
    "--prefix=${placeholder "out"}"
    "--with-arch=${arch}"
    "--with-abi=${abi}"
  ];

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
  ];


  buildPhase = ''
    runHook preBuild

    echo "DBG"
    echo $OBJCOPY

    unset AR AS LD OBJCOPY OBJDUMP

    echo $OBJCOPY

    false
    export PATH=${placeholder "out"}/bin:$PATH
    make -j$NIX_BUILD_CORES $makeFlags newlib
    make -j$NIX_BUILD_CORES $makeFlags linux

    runHook postBuild
  '';

  # riscv-gnu-toolchain installs everything in the build rules, so we just do
  # nothing here
  installPhase = "true";
}
