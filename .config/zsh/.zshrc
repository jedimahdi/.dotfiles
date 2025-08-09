# zmodload zsh/zprof
export ZSHARE="$XDG_DATA_HOME/zsh"
export HISTFILE="$ZSHARE/zsh_history"
export HISTSIZE="5000"
export SAVEHIST=$HISTSIZE
export LS_COLORS='no=0:fi=0:di=34:ex=32'

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

autoload -Uz compinit colors vcs_info edit-command-line select-word-style
compinit -C -d "$ZSHARE/.zcompdump"
colors
select-word-style bash # only alphanumeric chars are considered WORDCHARS

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
alias gdb="gdb --silent"

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

# alias gs="git status --short"
alias gc="git commit"
alias ga="git add"
alias gap="git add --patch"
# alias gl='git log --graph --all --pretty=format:"%C(magenta)%h %C(white) %an  %ar%C(blue)  %D%n%s%n"'
alias gd="git diff --output-indicator-new=' ' --output-indicator-old=' '"
alias gds="gd --staged"
alias lg='lazygit'
alias gcl="git clone --depth 1"

# open commands in $EDITOR with C-x C-e
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

# Make completion:
# - Try exact (case-sensitive) match first.
# - Then fall back to case-insensitive.
# - Accept abbreviations after . or _ or - (ie. f.b -> foo.bar).
# - Substring complete (ie. bar -> foobar).
zstyle ':completion:*' matcher-list '' '+m:{[:lower:]}={[:upper:]}' '+m:{[:upper:]}={[:lower:]}' '+m:{_-}={-_}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

# Colorize completions using default `ls` colors.
# zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"  # colored files and directories, blue selection box
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"

zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:git:*' formats 'on %F{magenta} %b%f'
zstyle ':vcs_info:git:*' actionformats 'on %F{magenta} %b|%a%f'
zstyle ':vcs_info:*:*' check-for-changes false

precmd_functions+=(vcs_info)

prompt_dir() {
  if [[ -n $vcs_info_msg_0_ ]]; then
    print -Pn "%1~"
  else
    print -Pn "%2~"
  fi
}

setopt prompt_subst

PROMPT='
%F{cyan}$(prompt_dir)%f ${vcs_info_msg_0_}
%(?.%F{green}❯.%F{red}❯)%f '

source <(fzf --zsh)
eval "$(zoxide init zsh --cmd cd)"

source "$ZSHARE/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh"
source "$ZSHARE/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
# zprof
