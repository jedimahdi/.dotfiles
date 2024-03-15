{ inputs, ... }: {
  imports = [
    inputs.stylix.homeManagerModules.stylix
    inputs.nix-index-database.hmModules.nix-index
    ./stylix
    ./terminal
  ];

  nixpkgs.config.allowUnfree = true;
  systemd.user.startServices = "sd-switch";
  programs.home-manager.enable = true;

  home = {
    stateVersion = "23.11";
    username = "mahdi";
    homeDirectory = "/home/mahdi";
  };

  manual = {
    html.enable = false;
    json.enable = false;
    manpages.enable = true;
  };
}
