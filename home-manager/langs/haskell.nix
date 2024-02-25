{ pkgs, ... }: {
  home.packages = with pkgs; [
    haskell.compiler.ghc948
    haskellPackages.cabal-install
    haskellPackages.haskell-language-server
    haskellPackages.hoogle
    haskellPackages.fourmolu
    haskellPackages.cabal-fmt
  ];
}
