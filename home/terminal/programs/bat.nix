{ pkgs, ... }: {
  programs.bat = {
    enable = true;
    config = {
      theme = "base16";
      pager = "less -FR";
    };
    extraPackages = with pkgs.bat-extras; [ batman ];
  };
  stylix.targets.bat.enable = false;
}
