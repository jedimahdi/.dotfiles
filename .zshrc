# zmodload zsh/zprof

PROMPT='%F{cyan}%1~%f %(?.%F{white}❯.%F{red}❯)%f '

ZSH_DATA_DIR="${XDG_DATA_HOME:-$HOME/.local/share}/zsh"
HISTFILE="$ZSH_DATA_DIR/zsh_history"
SAVEHIST=2000
HISTSIZE=2200

setopt INTERACTIVE_COMMENTS
setopt NO_BEEP
setopt NO_FLOW_CONTROL

setopt INC_APPEND_HISTORY
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_REDUCE_BLANKS
setopt HIST_IGNORE_SPACE

zshaddhistory() {
  local cmd="${1%%$'\n'}"
  ((${#cmd} > 200 || ${#cmd} <= 2)) && return 1
  [[ "$cmd" =~ '^(clear|tsession|pwd|exit)$' ]] && return 1
  [[ "$cmd" =~ '^cd\s' ]] && return 2
  return 0
}

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
alias l='ls -1A'
alias la='ls -gAh --time-style=long-iso'

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
alias hl='rg --passthru'

alias ta='tmux attach'
alias tl='tmux list-sessions'
alias tn='tmux new-session -s'
alias tt='tsession'

alias pi='sudo pacman -S --needed'
alias pu='sudo pacman -Sy --needed archlinux-keyring && sudo pacman -Su'
alias ppu='sudo proxychains pacman -Sy --needed archlinux-keyring && sudo proxychains pacman -Su'
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
alias git-repo='firefox "$(git remote get-url origin | sed -e "s/git@\(.*\):/https:\/\/\1\//" -e "s/\.git$//")"'

alias ctree='systemd-cgls --user'
alias sc='systemctl --user'
alias ssh-github='ssh -T git@github.com'
alias python-http-server="python -m http.server"
alias d='date "+%Y-%m-%d %A"; LC_TIME=fa_IR.UTF-8 date "+%Y-%m-%d"; date "+%H:%M:%S"'
alias lf='lfcd'

e() {
  command nvim "${1:-.}"
}

se() {
  sudo -E nvim "${1:-.}"
}

ef() {
  local file
  file=$(rg --files --hidden -g '!node_modules/' -g '!.git/' -g '!target/' | fzf --scheme="path") || return
  command nvim "$file"
}

ptree() {
  ps --user "$USER" -o pid,cmd --no-headers --forest |
    grep -v firefox |
    sed -e 's/\\_/├─/g' -e 's/|/│/g' |
    less -R
}

y() {
  local tmp cwd
  tmp="$(mktemp -t yazi-cwd.XXXXXX)" || return
  yazi "$@" --cwd-file="$tmp"
  if cwd="$(cat -- "$tmp")" && [[ -n $cwd && $cwd != $PWD ]]; then
    builtin cd -- "$cwd"
  fi
  command rm -f -- "$tmp"
}

lfcd() {
  cd "$(command lf -print-last-dir "$@")"
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

export MANPAGER='nvim +Man!'
export ESCDELAY=25
export LESS='-RQKcig -j.5 --incsearch --no-vbell -x4 --use-color -DPw -DEw'
export FZF_DEFAULT_OPTS="--style minimal \
  --info inline-right --color 'bg+:-1,fg+:15,gutter:-1,pointer:4,border:8' \
  --layout=reverse --height 50% --prompt '❯ ' --gutter ' ' \
  --bind 'ctrl-d:preview-half-page-down,ctrl-u:preview-half-page-up,ctrl-e:preview-down'"

source <(fzf --zsh)

# if [[ -z ${SSH_CONNECTION:-} ]]; then
#   export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
# fi

# zprof
