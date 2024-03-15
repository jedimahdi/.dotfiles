{ ... }:
{
  imports = [
    ./programs
    ./shell/starship.nix
    ./shell/sh.nix
  ];

  programs = {
    aria2.enable = true;
    thefuck.enable = true;
    nix-index.enable = true;
    nix-index-database.comma.enable = true;
  };

  home.sessionVariables = {
    EDITOR = "nvim";
  };
}
