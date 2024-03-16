{ config, ... }: {
  programs.fuzzel = {
    enable = true;
    settings = {
      main = {
        width = 60;
        terminal = "alacritty -e";
        icon-theme = "${config.gtk.iconTheme.name}";
        line-height = 36;
      };
      border = {
        width = 0;
        radius = 4;
      };
      key-bindings = {
        next = "Down Control+n Control+j";
        prev = "Up Control+p Control+k";
        delete-line = "none";
        delete-prev-word = "Mod1+BackSpace Control+BackSpace Control+w";
        first = "Control+s";
        last = "Control+z";
      };
    };
  };

}
