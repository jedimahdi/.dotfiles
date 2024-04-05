{ pkgs, ... }:
let
  aliases = {
    cbuild = "cabal build --enable-tests --enable-benchmarks --write-ghc-environment-files=always -O0";
    ctest = "cabal test --enable-tests --test-show-details=direct -O0";
    crepl = "cabal repl --build-depends pretty-simple";
    cbench = "cabal bench --enable-benchmarks -O0";
    crun = "cabal run -O0";
  };
in
{
  home.packages = with pkgs; [
    ghc
    haskellPackages.cabal-install
    haskellPackages.haskell-language-server
    haskellPackages.hoogle
    haskellPackages.fourmolu
    haskellPackages.cabal-fmt
  ];

  # home.file.".ghc/ghci.conf".text = ''
  #   :set -interactive-print=Text.Pretty.Simple.pPrint
  # '';

  programs.zsh.shellAliases = aliases;
  programs.bash.shellAliases = aliases;
}
