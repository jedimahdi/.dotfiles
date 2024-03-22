{ pkgs, ... }: {
  home.packages = [ pkgs.lazygit pkgs.zsh-forgit ];

  programs.git = {
    enable = true;
    package = pkgs.gitFull;
    userName = "Mahdi Seyedan";
    userEmail = "mahdi.se@yahoo.com";
    ignores = [ "*~" "*.swp" "result" "result-*" ".direnv" "node_modules" ];
    delta = {
      enable = true;
      options = {
        light = false;
        navigate = true;
        true-color = "always";
        side-by-side = true;
      };
    };
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
      af = "!git add $(git ls-files -m -o --exclude-standard | fzf -m)";
      update = "!git fetch upstream && git rebase upstream/`git rev-parse --abbrev-ref HEAD`";
    };

    extraConfig = {
      init.defaultBranch = "main";
      core.editor = "nvim";
    };
  };

  programs.gitui = {
    enable = true;
    keyConfig = ''
      move_left: Some(( code: Char('h'), modifiers: ( bits: 0,),)),
      move_right: Some(( code: Char('l'), modifiers: ( bits: 0,),)),
      move_up: Some(( code: Char('k'), modifiers: ( bits: 0,),)),
      move_down: Some(( code: Char('j'), modifiers: ( bits: 0,),)),
      stash_open: Some(( code: Char('l'), modifiers: ( bits: 0,),)),
      open_help: Some(( code: F(1), modifiers: ( bits: 0,),)),
    '';
  };
}
