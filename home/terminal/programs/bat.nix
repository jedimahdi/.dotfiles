{ pkgs, ... }: {
  programs.bat = {
    enable = true;
    config = {
      pager = "less -FR";
    };
    extraPackages = with pkgs.bat-extras; [ batman ];
  };
  stylix.targets.bat.enable = true;
}
