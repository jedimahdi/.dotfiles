{ pkgs, ... }: {
  home.packages = with pkgs; [
    haskell.compiler.ghc948
    haskellPackages.cabal-install
    haskellPackages.haskell-language-server
    haskellPackages.hoogle
    haskellPackages.fourmolu
    haskellPackages.cabal-fmt
  ];

  home.file.".ghc/ghci.conf".text = ''
    :set -interactive-print=Text.Pretty.Simple.pPrint
  '';
}
