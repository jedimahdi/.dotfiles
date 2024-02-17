{ lib, pkgs, ... }:
let
  myAliases = {
    ls = "eza --group-directories-first";
    l = "eza -la --icons --no-user --no-permissions --no-filesize --no-time --group-directories-first";
    tree = "eza --tree --icons --group-directories-first";
    c = "clear";
    cat = "bat";
    tc = "tmux-sessionizer";
    ta = "tmux attach";
    ".." = "cd ..";
    "..." = "cd ../..";
    "...." = "cd ../../..";
    fetch = "neofetch";
    lg = "lazygit";
    xdg-open = "${pkgs.mimeo}/bin/mimeo";
  };
in
{
  imports = [ ./starship.nix ];
  programs = {
    zsh = {
      enable = true;
      autocd = true;
      dotDir = ".config/zsh";
      enableAutosuggestions = true;
      enableCompletion = true;
      syntaxHighlighting.enable = true;
      defaultKeymap = "emacs";
      shellAliases = myAliases;
      initExtra = ''
        export PATH="$HOME/.dotfiles/bin:$PATH"
        export MANPAGER='nvim +Man!'

        # C-Backspace for word deletions
        bindkey "^H" backward-kill-word

        # open commands in $EDITOR with C-x C-e
        autoload -z edit-command-line
        zle -N edit-command-line
        bindkey "^X^E" edit-command-line
      '';
    };
    bash = {
      enable = true;
      enableCompletion = true;
      shellAliases = myAliases;
    };
    nushell = {
      enable = true;
      shellAliases = myAliases;
    };
    direnv = {
      enable = true;
      enableZshIntegration = true;
      nix-direnv.enable = true;
    };
    fzf = {
      enable = true;
      enableZshIntegration = true;
      colors = {
        "bg+" = "-1";
      };
    };
    bat = {
      enable = true;
      config = {
        theme = "ansi";
        style = "header";
      };
    };
    zoxide = {
      enable = true;
      enableZshIntegration = true;
      enableNushellIntegration = false;
      options = [ "--cmd" "cd" ];
    };
    jq.enable = true;
    htop.enable = true;
    less.enable = true;
  };

  home.packages = with pkgs; [
    neofetch
    lolcat
    gnugrep
    gnused
    bat
    eza
    bottom
    fd
    direnv
    nix-direnv
    wl-clipboard
    killall
    brightnessctl
    unzip
    neovim
    mdcat
  ];

  xdg.mimeApps.defaultApplications =
    let
      code = [
        "text/english"
        "text/plain"
        "text/x-makefile"
        "text/x-c++hdr"
        "text/x-c++src"
        "text/x-chdr"
        "text/x-csrc"
        "text/x-java"
        "text/x-moc"
        "text/x-pascal"
        "text/x-tcl"
        "text/x-tex"
        "application/x-shellscript"
        "text/x-c"
        "text/x-c++"
      ];
    in
    lib.genAttrs code (_: [ "nvim.desktop" ]);
}
