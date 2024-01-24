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
      la = "!git config -l | grep alias | cut -c 7-";
      ignore = "!gi() { curl -sL https://www.gitignore.io/api/$@ ;}; gi";
      l = "log --graph --pretty=format:'%Cred%h%Creset %C(bold blue)%an%C(reset) - %s - %Creset %C(yellow)%d%Creset %Cgreen(%cr)%Creset' --abbrev-commit --date=relative";
      s = "status --short";
      ss = "status";
      graph = "log --all --decorate --graph --oneline";
      undo = "reset --soft HEAD~1";
      amend = "commit -a --amend";
      count = "shortlog -sn";
      review = "!git log --no-merges --pretty=%an | head -n 100 | sort | uniq -c | sort -nr";
      day = "!sh -c 'git log --reverse --no-merges --branches=* --date=local --after=\"yesterday 11:59PM\" --author=\"`git config --get user.name`\"'";
      update = "!git fetch upstream && git rebase upstream/`git rev-parse --abbrev-ref HEAD`";
    };

    extraConfig = {
      init.defaultBranch = "main";
    };
  };
}
