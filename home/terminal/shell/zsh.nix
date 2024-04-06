{ config, ... }:
{
  programs.zsh = {
    enable = true;
    autocd = true;
    dotDir = ".config/zsh";
    autosuggestion.enable = true;
    enableCompletion = true;
    syntaxHighlighting.enable = true;
    zsh-abbr = {
      enable = true;
      abbreviations = {
        g = "git";
        n = "npm";
        j = "just";
      };
    };
    history = {
      path = "${config.xdg.dataHome}/zsh/zsh_history";
      expireDuplicatesFirst = true;
    };
    defaultKeymap = "emacs";
    initExtra = ''
      export PATH="$HOME/.dotfiles/bin:$PATH"
      export MANPAGER='nvim +Man!'

      # Disable C-s freezing the terminal
      stty -ixon

      # C-Backspace for word deletions
      bindkey "^H" backward-kill-word

      # open commands in $EDITOR with C-x C-e
      autoload -z edit-command-line
      zle -N edit-command-line
      bindkey "^X^E" edit-command-line

      # case insensitive tab completion
      zstyle ':completion:*' completer _complete _ignored _approximate
      zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
      zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
      zstyle ':completion:*' menu select
      zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
      zstyle ':completion:*' verbose true
    '';
  };
}
