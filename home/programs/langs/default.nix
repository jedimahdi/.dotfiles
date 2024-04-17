{ pkgs, ... }:
{
  imports = [
    ./haskell.nix
    ./c.nix
    ./go.nix
    ./js.nix
    ./rust.nix
    ./ocaml.nix
  ];
  home.packages = with pkgs; [
    nodePackages.bash-language-server
    shellcheck
    shfmt
    stylua
    lua-language-server
    nil
    nixpkgs-fmt
    alejandra
    statix
    deadnix
    manix
  ];
}
