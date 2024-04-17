{ pkgs, ... }: {
  home.packages = [ pkgs.zsh-forgit ];

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

  programs.lazygit = {
    enable = true;
    settings = {
      gui = {
        nerdFontsVersion = 3;
        showRandomTip = false;
        showCommandLog = false;
        theme = {
          selectedLineBgColor = [ "default" ];
        };
      };
      update = {
        method = "never";
      };
    };
  };

  programs.gitui = {
    enable = true;
    keyConfig = ''
      (
        open_help: Some(( code: F(1), modifiers: "")),

        move_left: Some(( code: Char('h'), modifiers: "")),
        move_right: Some(( code: Char('l'), modifiers: "")),
        move_up: Some(( code: Char('k'), modifiers: "")),
        move_down: Some(( code: Char('j'), modifiers: "")),

        popup_up: Some(( code: Char('p'), modifiers: "CONTROL")),
        popup_down: Some(( code: Char('n'), modifiers: "CONTROL")),
        page_up: Some(( code: Char('b'), modifiers: "CONTROL")),
        page_down: Some(( code: Char('f'), modifiers: "CONTROL")),
        home: Some(( code: Char('g'), modifiers: "")),
        end: Some(( code: Char('G'), modifiers: "SHIFT")),
        shift_up: Some(( code: Char('K'), modifiers: "SHIFT")),
        shift_down: Some(( code: Char('J'), modifiers: "SHIFT")),

        edit_file: Some(( code: Char('I'), modifiers: "SHIFT")),

        status_reset_item: Some(( code: Char('U'), modifiers: "SHIFT")),

        diff_reset_lines: Some(( code: Char('u'), modifiers: "")),
        diff_stage_lines: Some(( code: Char('s'), modifiers: "")),

        stashing_save: Some(( code: Char('w'), modifiers: "")),
        stashing_toggle_index: Some(( code: Char('m'), modifiers: "")),

        stash_open: Some(( code: Char('l'), modifiers: "")),

        abort_merge: Some(( code: Char('M'), modifiers: "SHIFT")),
      )
    '';
    theme = ''
      (
        selected_tab: Some(Reset),
        selection_bg: Some(Reset),
        cmdbar_bg: Some(Reset),
        cmdbar_extra_lines_bg: Some(Reset),
      )
    '';
  };
}
