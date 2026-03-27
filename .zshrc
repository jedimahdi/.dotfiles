# zmodload zsh/zprof

export ZSHARE="${XDG_DATA_HOME:-$HOME/.local/share}/zsh"

export HISTFILE="$ZSHARE/zsh_history"
export HISTSIZE=10000
export SAVEHIST=$HISTSIZE
export LS_COLORS='no=0:fi=0:di=34:ex=32'

if [[ -z ${SSH_CONNECTION:-} ]]; then
  export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
fi

export PATH="/home/mahdi/.gapcode/bin:$PATH"
typeset -U path PATH

[[ ! -o interactive ]] && return
[[ -d $ZSHARE ]] || mkdir -p "$ZSHARE"

autoload -Uz colors && colors
PROMPT='%F{cyan}%1~%f %(?.%F{white}❯.%F{red}❯)%f '

setopt INC_APPEND_HISTORY      # Immediately append to history file
setopt HIST_IGNORE_DUPS        # Ignore consecutive duplicates
setopt HIST_EXPIRE_DUPS_FIRST  # Expire duplicate entries first
setopt HIST_IGNORE_ALL_DUPS    # Remove older duplicate entries
setopt HIST_FIND_NO_DUPS       # Don't show duplicates when searching
setopt HIST_REDUCE_BLANKS      # Remove superfluous blanks

bindkey -e

autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey '^p' up-line-or-beginning-search
bindkey '^n' down-line-or-beginning-search

[[ -t 0 ]] && stty -ixon

autoload -Uz compinit
compinit -C -d "$ZSHARE/.zcompdump"

autoload -Uz select-word-style
select-word-style shell

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias c='clear'

if (( $+commands[eza] )); then
  alias ls='eza --all --group-directories-first'
  alias l='eza --long --no-time --no-user --no-permissions --group-directories-first'
  alias la='eza --long --octal-permissions --time-style="+%Y-%m-%d %H:%M" --group-directories-first'
  alias lt='eza --tree'
fi

alias mv='mv -iv'
alias rm='rm -vI'
alias cp='cp -iv'
alias bc='bc -ql'
alias gdb='gdb --silent'
alias grep='grep --color=auto'
alias diff='diff --color=auto'
alias ip='ip -color=auto'
alias df='df -h'

alias ta='tmux attach'
alias tl='tmux list-sessions'
alias tn='tmux new-session -s'
alias tc='tsession'
alias ts='tconnect "$PWD" scratch'

alias pi='sudo pacman -S --needed'
alias pu='sudo pacman -Sy --needed archlinux-keyring && sudo pacman -Su'
alias pf='pacman -Ss'
alias pr='sudo pacman -Rns'
(( $+commands[fzf] )) && alias fpac='/usr/bin/pacman -Slq | fzf --preview "/usr/bin/pacman -Si {}" --layout=reverse'

alias gs='git status'
alias gss='gitar status --fzf'
alias gc='git commit'
alias ga='git add'
alias gap='git add --patch'
alias gl='gitar log --fzf'
alias gd='git diff'
alias gds='gd --staged'
alias lg='lazygit'
alias gcl='git clone --depth 1'

alias ptree='ps --user "$USER" -o pid,cmd --no-headers --forest | grep -v firefox | sed -e "s/\\_/├─/g" -e "s/|/│/g" | less -R'
alias ctree='systemd-cgls --user'
alias sc='systemctl --user'

alias d='date "+%Y-%m-%d %A"; LC_TIME=fa_IR.UTF-8 date "+%Y-%m-%d"; date "+%H:%M:%S"'

function e() {
  command nvim "${1:-.}"
}

function ef() {
  (( $+commands[rg] && $+commands[fzf] && $+commands[nvim] )) || return
  local file
  file=$(rg --files --hidden -g '!node_modules/' -g '!.git/' -g '!target/' | fzf) || return
  command nvim "$file"
}

function help() {
  (( $# )) || return
  if (( $+commands[bat] )); then
    command "$@" --help 2>&1 | bat --language=help
  else
    command "$@" --help
  fi
}
compdef _command help

function y() {
  (( $+commands[yazi] )) || return
  local tmp cwd
  tmp="$(mktemp -t yazi-cwd.XXXXXX)" || return
  yazi "$@" --cwd-file="$tmp"
  if cwd="$(cat -- "$tmp")" && [[ -n $cwd && $cwd != $PWD ]]; then
    builtin cd -- "$cwd"
  fi
  rm -f -- "$tmp"
}

autoload -U edit-command-line
zle -N edit-command-line
bindkey '^x^e' edit-command-line

fzf-history-widget() {
  (( $+commands[fzf] )) || return
  local selected
  selected=$(fc -rl 1 | awk '{$1=""; print substr($0,2)}' | fzf --tiebreak=index --query="$LBUFFER")
  if [[ -n $selected ]]; then
    LBUFFER=$selected
  fi
  zle reset-prompt
}
zle -N fzf-history-widget
bindkey '^R' fzf-history-widget

zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'
zstyle ':completion:*' completer _complete
zstyle ':completion:*' file-sort modification
zstyle ':completion:*' list-dirs-first yes
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"

# zprof
