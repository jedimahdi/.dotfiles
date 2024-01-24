{
  config,
  pkgs,
  ...
}: {
  home.packages = [pkgs.git pkgs.lazygit];
  programs.git = {
    enable = true;
    userName = "Mahdi Seyedan";
    userEmail = "mahdi.se@yahoo.com";
    ignores = ["*~" "*.swp" "result" "result-*" ".direnv" "node_modules"];
    aliases = {
      s = "status --short";
      ss = "status";
      graph = "log --all --decorate --graph --oneline";
    };

    extraConfig = {
      init.defaultBranch = "main";
    };
  };
}
