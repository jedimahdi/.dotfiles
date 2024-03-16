{ pkgs, ... }:
let
  aliases = {
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
    less = ''${pkgs.bat}/bin/bat --paging=always --pager "${pkgs.less}/bin/less -RF"'';
  };
in
{
  programs = {
    zsh.shellAliases = aliases;
    bash.shellAliases = aliases;
    nushell.shellAliases = aliases;
  };
}
