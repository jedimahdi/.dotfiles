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
    gtk3.extraConfig = {
      Settings = ''
        gtk-application-prefer-dark-theme=1
      '';
    };
    gtk4.extraConfig = {
      Settings = ''
        gtk-application-prefer-dark-theme=1
      '';
    };
  };
  qt = {
    enable = true;
    platformTheme = "gtk";
    style = {
      name = "adwaita-dark";
      package = pkgs.adwaita-qt;
    };
  };
}
