{
  config,
  pkgs,
  ...
}: {
  home.packages = [pkgs.qutebrowser];
  programs.qutebrowser = {
    enable = true;
  };
}
