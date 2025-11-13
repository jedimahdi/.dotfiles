[[ $- != *i* ]] && return

# zmodload zsh/zprof
export ZSHARE="$XDG_DATA_HOME/zsh"
export HISTFILE="$ZSHARE/zsh_history"
export HISTSIZE="5000"
export SAVEHIST=$HISTSIZE
export LS_COLORS='no=0:fi=0:di=34:ex=32'

if [[ -z "${SSH_CONNECTION}" ]]; then
  export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
fi

# setopt SHARE_HISTORY          # Share history across sessions
# setopt INC_APPEND_HISTORY     # Immediately append to history file
# setopt EXTENDED_HISTORY       # Record timestamp in history
setopt HIST_IGNORE_DUPS      # ignore consecutive duplicates
setopt HIST_EXPIRE_DUPS_FIRST # Expire duplicate entries first
setopt HIST_IGNORE_ALL_DUPS   # Remove older duplicate entries
setopt HIST_FIND_NO_DUPS      # Don't show duplicates when searching
setopt HIST_REDUCE_BLANKS     # Remove superfluous blanks

bindkey -e

autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey '^p' up-line-or-beginning-search
bindkey '^n' down-line-or-beginning-search

# Disable Ctrl+S freeze
stty -ixon

autoload -Uz compinit colors edit-command-line select-word-style
compinit -C -d "$ZSHARE/.zcompdump"
colors
select-word-style shell

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'

alias c='clear'


# alias ls="ls -hNA --color=auto --group-directories-first"
# alias l="ls -l"

alias ls='eza --all --icons --group-directories-first'
alias l='ls --long --octal-permissions --no-time --no-user --no-permissions'
alias la='ls --long --octal-permissions --time-style="+%Y-%m-%d %H:%M"'
alias lt='eza --icons=auto --tree'

alias mv='mv -iv'
alias rm='rm -vI'
alias cp='cp -iv'
alias bc="bc -ql"
alias gdb="gdb --silent"
alias grep='grep --color=auto'
alias diff='diff --color=auto'
alias ip='ip -color=auto'

alias ta='tmux attach'
alias tl='tmux list-sessions'
alias tn='tmux new-session -s'
alias tc='tsession'
alias ts='tconnect $PWD scratch'

alias pi='sudo pacman -S --needed'
alias pu='sudo pacman -Sy --needed archlinux-keyring && sudo pacman -Su'
alias pf='pacman -Ss'
alias pr='sudo pacman -Rs'
alias fpac="/usr/bin/pacman -Slq | fzf --preview '/usr/bin/pacman -Si {}' --layout=reverse"

alias gs="gitar status --fzf"
alias gc="git commit"
alias ga="git add"
alias gap="git add --patch"
alias gl='gitar log --fzf'
alias gd="git diff --output-indicator-new=' ' --output-indicator-old=' '"
alias gds="gd --staged"
alias lg='lazygit'
alias gcl="git clone --depth 1"

alias ptree="ps --user \"$USER\" -o pid,cmd --no-headers --forest | grep -v firefox | sed -e 's/\\\\_/├─/g' -e 's/|/│/g' | less -R"
alias ctree="systemd-cgls --user"
alias sc='systemctl --user'

alias dx='date +"%Y-%m-%d %H:%M:%S %A"; LC_TIME=fa_IR.UTF-8 date +"%Y-%m-%d"'

e() { nvim "${1:-.}"; }
ef() {
  local file
  file=$(rg --files --hidden -g '!node_modules/' -g '!.git/' -g '!target/' | fzf) || return
  nvim "$file"
}

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

fzf-history-widget() {
  local selected
  selected=$(fc -rl 1 | awk '{$1=""; print substr($0,2)}' |
    fzf --tiebreak=index --query="$LBUFFER")
  if [[ -n $selected ]]; then
    LBUFFER=$selected
  fi
  zle reset-prompt
}
zle -N fzf-history-widget
bindkey '^R' fzf-history-widget

# Make completion:
# - Try exact (case-sensitive) match first.
# - Then fall back to case-insensitive.
# - Accept abbreviations after . or _ or - (ie. f.b -> foo.bar).
# - Substring complete (ie. bar -> foobar).
# zstyle ':completion:*' matcher-list '' '+m:{[:lower:]}={[:upper:]}' '+m:{[:upper:]}={[:lower:]}' '+m:{_-}={-_}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'
zstyle ':completion:*' completer _complete

zstyle ':completion:*' file-sort modification  # show recently used files first
zstyle ':completion:*' list-dirs-first yes
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*' ignored-patterns '.git'

zstyle ':completion:*' rehash false  # improves performance
zstyle ':completion:*' use-cache true
zstyle ':completion:*' cache-path "$ZSHARE/.zcompcache"

# PROMPT='
# %F{cyan}%2~%f
# %(?.%F{green}❯.%F{red}❯)%f '

PROMPT='%(?.%F{green}➜.%F{red}➜)%f %B%F{cyan}%1~%f%b '

# eval "$(zoxide init zsh --cmd cd)"
# source "$ZSHARE/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"

# autoload -Uz add-zsh-hook
# load_plugins() {
#   # source <(fzf --zsh)
#   eval "$(zoxide init zsh --cmd cd)"
#
#   # source "$ZSHARE/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh"
#   source "$ZSHARE/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
#
#   add-zsh-hook -d precmd load_plugins
# }
# add-zsh-hook precmd load_plugins
# zprof
