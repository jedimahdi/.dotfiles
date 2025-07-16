# zmodload zsh/zprof
export ZSHARE="$XDG_DATA_HOME/zsh"
export ZSHRC="${XDG_CONFIG_HOME:-$HOME/.config}/zsh/.zshrc"
export HISTFILE="$ZSHARE/zsh_history"
export HISTSIZE="10000"
export SAVEHIST="10000"

if [[ -z "${SSH_CONNECTION}" ]]; then
  export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
fi

setopt SHARE_HISTORY          # Share history across sessions
setopt INC_APPEND_HISTORY     # Immediately append to history file
setopt EXTENDED_HISTORY       # Record timestamp in history
setopt HIST_EXPIRE_DUPS_FIRST # Expire duplicate entries first
setopt HIST_IGNORE_ALL_DUPS   # Remove older duplicate entries
setopt HIST_FIND_NO_DUPS      # Don't show duplicates when searching
setopt HIST_REDUCE_BLANKS     # Remove superfluous blanks

bindkey -e
bindkey '^p' history-search-backward
bindkey '^n' history-search-forward

# Disable Ctrl+S freeze
stty -ixon

if [[ ! -f "${XDG_DATA_HOME:-$HOME/.local/share}/zinit/zinit.git/zinit.zsh" ]]; then
  mkdir -p "${XDG_DATA_HOME:-$HOME/.local/share}/zinit"
  git clone https://github.com/zdharma-continuum/zinit "${XDG_DATA_HOME:-$HOME/.local/share}/zinit/zinit.git"
fi

source "${XDG_DATA_HOME:-$HOME/.local/share}/zinit/zinit.git/zinit.zsh"

zcompdump="$ZSHARE/.zcompdump"
autoload -Uz compinit
autoload -Uz zrecompile
if [[ ! -s $zcompdump ]]; then
  compinit -i -d "$zcompdump"
  [[ -f "$zcompdump" ]] && zrecompile -p "$zcompdump"
else
  compinit -C -d "$zcompdump"
fi

zinit light zsh-users/zsh-autosuggestions
zinit light zsh-users/zsh-syntax-highlighting

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'

alias c='clear'

alias ls='eza --all --icons --group-directories-first'
alias l='ls --long --octal-permissions --no-time --no-user --no-permissions'
alias la='ls --long --octal-permissions --time-style="+%Y-%m-%d %H:%M"'

alias mv='mv -iv'
alias rm='rm -vI'
alias cp='cp -iv'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
alias grep='grep --color=auto'
alias bc="bc -ql"

alias ta='tmux attach'
alias tl='tmux list-sessions'
alias tn='tmux new-session -s'
alias tc='tsession'
alias ts='tconnect $PWD scratch'

alias pi='sudo pacman -S --needed'
alias pu='sudo pacman -Syu'
alias pf='pacman -Ss'
alias pr='sudo pacman -Rs'
alias fpac="/usr/bin/pacman -Slq | fzf --preview '/usr/bin/pacman -Si {}' --layout=reverse"

alias gdb="gdb --silent"
alias gs="git status --short"
alias gc="git commit"
alias ga="git add"
alias gap="git add --patch"
alias gl='git log --graph --all --pretty=format:"%C(magenta)%h %C(white) %an  %ar%C(blue)  %D%n%s%n"'
alias gd="git diff --output-indicator-new=' ' --output-indicator-old=' '"
alias gds="gd --staged"
alias lg='lazygit'
alias gcl="git clone --depth 1"

# open commands in $EDITOR with C-x C-e
autoload -U edit-command-line
zle -N edit-command-line
bindkey "^x^e" edit-command-line

edit-last-command-output() {
  if [[ "$TERM" =~ "tmux" ]]; then
    tmux capture-pane -p -S - -E - -J | tac | awk '
      found && !/❯/ { print }
      /❯/ && !found { found=1; next }
      /❯/ && found {exit}
    ' | tac | nvim -
  else
    echo
    print -Pn "%F{red}error: can't capture last command output outside of tmux%f"
    zle accept-line
  fi
}

zle -N edit-last-command-output
bindkey '^x^o' edit-last-command-output

function y() {
  local tmp="$(mktemp -t "yazi-cwd.XXXXXX")"
  yazi "$@" --cwd-file="$tmp"
  if cwd="$(cat -- "$tmp")" && [ -n "$cwd" ] && [ "$cwd" != "$PWD" ]; then
    builtin cd -- "$cwd"
  fi
  rm -f -- "$tmp"
}

eval "$(starship init zsh)"
source <(fzf --zsh)
eval "$(zoxide init zsh --cmd cd)"
# zprof
