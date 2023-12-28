{ config, pkgs, ... }:

let
  myAliases = {
    ls = "eza --group-directories-first";
    l = "eza -la --icons --no-user --no-permissions --no-filesize --no-time --group-directories-first";
    c = "clear";
    cat = "bat";
    tc = "tmux-sessionizer";
    ta = "tmux attach";
    ".." = "cd ..";
    "..." = "cd ../..";
    "...." = "cd ../../..";
    fetch = "neofetch";
    lg = "lazygit";
  };
in
{
  imports = [ ./starship.nix ];

  programs.zsh = {
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

  programs.bat = {
    enable = true;
    config = {
      theme = "ansi";
      style = "header";
    };
  };
}
