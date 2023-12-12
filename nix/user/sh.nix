{ config, pkgs, ... }:

let
  myAliases = {
    ls = "eza --group-directories-first";
    l = "eza --icons -l -T -L=1";
    cat = "bat";
    c = "clear";
    tc = "tmux-sessionizer";
    ta = "tmux attach";
    ".." = "cd ..";
    "..." = "cd ../..";
    "...." = "cd ../../..";
    fetch = "neofetch";
  };
in
{
  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    syntaxHighlighting.enable = true;
    defaultKeymap = "emacs";
    shellAliases = myAliases;
    initExtra = ''
      PROMPT="%F{green}→%f "
      [ $TERM = "dumb" ] && unsetopt zle && PS1='$ '
      export PATH="$HOME/.dotfiles/bin:$PATH"
      export MANPAGER='nvim +Man!'
    '';
  };

  programs.bash = {
    enable = true;
    enableCompletion = true;
    shellAliases = myAliases;
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
  ];

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
    nix-direnv.enable = true;
  };

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };
}
