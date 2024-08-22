{ config, pkgs, ... }:
{
  programs.zsh = {
    enable = true;
    autocd = true;
    dotDir = ".config/zsh";
    defaultKeymap = "emacs";
    history = {
      path = "${config.xdg.dataHome}/zsh/zsh_history";
      expireDuplicatesFirst = true;
    };
    autosuggestion.enable = true;
    enableCompletion = true;
    syntaxHighlighting.enable = true;
    plugins = [
      {
        name = "fzf-tab";
        src = "${pkgs.zsh-fzf-tab}/share/fzf-tab";
      }
    ];

    initExtra = ''
      export PATH="$HOME/.dotfiles/bin:$PATH"
      # export MANPAGER='nvim +Man!'

      # Disable C-s freezing the terminal
      stty -ixon

      # C-Backspace for word deletions
      bindkey "^H" backward-kill-word

      bindkey '^p' history-search-backward
      bindkey '^n' history-search-forward

      # open commands in $EDITOR with C-x C-e
      autoload -z edit-command-line
      zle -N edit-command-line
      bindkey "^X^E" edit-command-line

      # Completion styling
      zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'
      zstyle ':completion:*' list-colors "$${(s.:.)LS_COLORS}"
      zstyle ':completion:*' menu no
      zstyle ':fzf-tab:complete:cd:*' fzf-preview 'eza -1 --color=always $realpath'
      zstyle ':completion:*:descriptions' format '[%d]'
      zstyle ':fzf-tab:*' switch-group '<' '>'

      autoload -U select-word-style
      select-word-style bash

      # [[ ! -r /home/mahdi/.opam/opam-init/init.zsh ]] || source /home/mahdi/.opam/opam-init/init.zsh  > /dev/null 2> /dev/null
    '';
  };
}
