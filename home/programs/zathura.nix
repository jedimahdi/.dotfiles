{ ... }: {
  programs.zathura = {
    enable = true;
    options = {
      # recolor-lightcolor = "rgba(0,0,0,0)";
      # default-bg = "rgba(0,0,0,0.7)";

      font = "Noto Sans 22";
      selection-notification = true;

      selection-clipboard = "clipboard";
      adjust-open = "best-fit";
      pages-per-row = "1";
      scroll-page-aware = "true";
      scroll-full-overlap = "0.01";
      scroll-step = "100";
      zoom-min = "10";
    };
  };

  xdg.mimeApps.defaultApplications = {
    "application/pdf" = [ "org.pwmt.zathura.desktop" ];
    "application/epub+zip" = [ "org.pwmt.zathura.desktop" ];
  };
}
