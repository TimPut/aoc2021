let 
  pkgs = import (builtins.fetchTarball {
    # Descriptive name to make the store path easier to identify
    name = "nixos-20.09";

    # Commit hash for nixos-20.09 as of 16 November 2021
    url = "https://github.com/NixOS/nixpkgs/archive/1c1f5649bb9c1b0d98637c8c365228f57126f361.tar.gz";

    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "0f2nvdijyxfgl5kwyb4465pppd5vkhqxddx6v40k2s0z9jfhj0xl";}) {};

    # Commit hash for nixos-20.09 as of 23 March 2021
    # url = "https://github.com/NixOS/nixpkgs/archive/f8929dce13e729357f31d5b2950cbb097744bed7.tar.gz";
    # sha256 = "06ikqdb5038vkkyx4hi5lw4gksjjndjg7mz0spawnb1gpzhqkavs";}) {};
in

pkgs.stdenv.mkDerivation rec {
  name = "kattis-dev-env";
  src = ./.;
  buildInputs = [
    (pkgs.haskell.packages.ghc884.ghcWithPackages (p: with p; [
      vector QuickCheck parallel containers unordered-containers MemoTrie ghc-core split smallcheck fgl heaps
      # ghc-vis
    ]))
    pkgs.cabal-install
    pkgs.haskellPackages.haskell-language-server


  ];
  libPath = pkgs.lib.makeLibraryPath buildInputs;
  shellHook = ''
    export LD_LIBRARY_PATH=${libPath}:$LD_LIBRARY_PATH
    export LANG=en_US.UTF-8
  '';
  LOCALE_ARCHIVE =
    if pkgs.stdenv.isLinux
    then "${pkgs.glibcLocales}/lib/locale/locale-archive"
    else "";
}
