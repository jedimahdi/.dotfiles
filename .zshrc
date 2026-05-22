# zmodload zsh/zprof

ZSH_DATA_DIR="${XDG_DATA_HOME:-$HOME/.local/share}/zsh"

PROMPT='%F{cyan}%1~%f %(?.%F{white}❯.%F{red}❯)%f '

setopt INTERACTIVE_COMMENTS
setopt NO_BEEP
setopt NO_FLOW_CONTROL

HISTFILE="$ZSH_DATA_DIR/zsh_history"
SAVEHIST=2000
HISTSIZE=2999
HISTORY_IGNORE="(c|ls|tc|l|lg)"

setopt HIST_IGNORE_ALL_DUPS
setopt HIST_SAVE_NO_DUPS
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_REDUCE_BLANKS
setopt APPEND_HISTORY
setopt INC_APPEND_HISTORY
setopt HIST_IGNORE_SPACE
# setopt HIST_IGNORE_DUPS

zshaddhistory() { (( ${#1} <= 2000 )) }

autoload -Uz compinit
compinit -C -d "$ZSH_DATA_DIR/.zcompdump"

zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'
zstyle ':completion:*' completer _complete

bindkey -e

autoload -Uz select-word-style
select-word-style shell

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias c='clear'

alias ls='ls --group-directories-first --color=auto'

if (( $+commands[eza] )); then
  alias ls='eza --icons --group-directories-first --color=auto'
  alias l='eza -la --icons --group-directories-first --no-time --no-user --no-permissions'
  alias la='eza -la --group-directories-first'
  alias ll='eza -lh --icons --git --group-directories-first'
  alias lt='eza --tree --level=2 --icons'
fi

alias mv='mv -iv'
alias rm='rm -vI --preserve-root'
alias cp='cp -iv'
alias bc='bc -ql'
alias gdb='gdb --silent'
alias grep='grep --color=auto'
alias diff='diff --color=auto -u'
alias ip='ip -color=auto'
alias df='df -h'
alias ping='ping -c 4'

alias ta='tmux attach'
alias tl='tmux list-sessions'
alias tn='tmux new-session -s'
alias tc='tsession'

alias pi='sudo pacman -S --needed'
alias pu='sudo pacman -Sy --needed archlinux-keyring && sudo pacman -Su'
alias pf='pacman -Ss'
alias pr='sudo pacman -Rns'
alias fpac='/usr/bin/pacman -Slq | fzf --preview "/usr/bin/pacman -Si {}" --layout=reverse'

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

alias ctree='systemd-cgls --user'
alias sc='systemctl --user'

alias d='date "+%Y-%m-%d %A"; LC_TIME=fa_IR.UTF-8 date "+%Y-%m-%d"; date "+%H:%M:%S"'

function e() {
  command nvim "${1:-.}"
}

function ef() {
  local file
  file=$(rg --files --hidden -g '!node_modules/' -g '!.git/' -g '!target/' | fzf --scheme="path") || return
  command nvim "$file"
}

function h() {
  (( $# )) || return
  if (( $+commands[bat] )); then
    command "$@" --help 2>&1 | bat --language=help
  else
    command "$@" --help
  fi
}
compdef _command h

ptree() {
  ps --user "$USER" -o pid,cmd --no-headers --forest \
    | grep -v firefox \
    | sed -e 's/\\_/├─/g' -e 's/|/│/g' \
    | less -R
}

function y() {
  local tmp cwd
  tmp="$(mktemp -t yazi-cwd.XXXXXX)" || return
  yazi "$@" --cwd-file="$tmp"
  if cwd="$(cat -- "$tmp")" && [[ -n $cwd && $cwd != $PWD ]]; then
    builtin cd -- "$cwd"
  fi
  command rm -f -- "$tmp"
}

autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey '^p' up-line-or-beginning-search
bindkey '^n' down-line-or-beginning-search

autoload -U edit-command-line
zle -N edit-command-line
bindkey '^x^e' edit-command-line

source <(fzf --zsh)

# if [[ -z ${SSH_CONNECTION:-} ]]; then
#   export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
# fi

# zprof
