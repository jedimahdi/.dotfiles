{ pkgs, ... }: {

  # xdg.configFile = {
  #   "gtk-3.0/gtk.css".source = ./gtk.css;
  #   "gtk-4.0/gtk.css".source = ./gtk.css;
  # };

  gtk = {
    enable = true;
    # theme = {
    #   name = "adw-gtk3-dark";
    #   package = pkgs.adw-gtk3;
    # };
    iconTheme = {
      name = "Papirus-Dark";
      package = pkgs.papirus-icon-theme;
    };
  };
  qt = {
    enable = true;
    platformTheme.name = "gtk";
    style = {
      name = "adwaita-dark";
      package = pkgs.adwaita-qt;
    };
  };
}
