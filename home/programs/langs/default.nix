{ pkgs, ... }:
{
  imports = [
    # ./haskell.nix
    ./c.nix
    ./go.nix
    ./js.nix
    ./rust.nix
    ./ocaml.nix
    ./zig.nix
  ];
  home.packages = with pkgs; [
    nodePackages.bash-language-server
    shellcheck
    shfmt
    nil
    nixpkgs-fmt
    alejandra
    statix
    deadnix
    manix

    python3

    lua5_1
    lua51Packages.luarocks
    stylua
    lua-language-server
  ];
}
