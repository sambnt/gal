{pkgs, config, ...}: {
  name = "gal";
  compiler-nix-name = "ghc982"; # Version of GHC to use

  # Cross compilation support:
  # crossPlatforms = p: pkgs.lib.optionals pkgs.stdenv.hostPlatform.isx86_64 ([
  #   # p.ghcjs
  # ]);

  # cabalProjectLocal = ''
  #   if arch(javascript)
  #     extra-packages: ghci
  # '';

  modules = [ ];

  # Tools to include in the development shell
  shell.tools.cabal = "latest";
  shell.tools.hlint = "latest";
  shell.tools.haskell-language-server = "latest";
  shell.tools.hoogle = "latest";
  shell.withHoogle = true;
  shell.packages = ps: builtins.attrValues (pkgs.haskell-nix.haskellLib.selectProjectPackages ps);
  shell.nativeBuildInputs = [ pkgs.buildPackages.cabalWrapped ];
}
