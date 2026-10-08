[[ $- != *i* ]] && return


PS1='\[\e[36m\]\W\[\e[0m\] \[\e[37m\]❯\[\e[0m\] '

shopt -s histappend

stty -ixon 2>/dev/null

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias ls='ls --group-directories-first --color=auto'
alias l='ls -1A'
alias la='ls -gAh --time-style=long-iso'
alias c='clear'

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
alias gds='git diff --staged'
alias lg='lazygit'
alias gcl='git clone --depth 1'
alias git-repo='firefox "$(git remote get-url origin | sed -e "s/git@\(.*\):/https:\/\/\1\//" -e "s/\.git$//")"'

alias ctree='systemd-cgls --user'
alias sc='systemctl --user'
alias ssh-github='ssh -T git@github.com'
alias python-http-server='python -m http.server'
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

  file=$(
    rg --files \
      --hidden \
      -g '!node_modules/' \
      -g '!.git/' \
      -g '!target/' |
      fzf --scheme=path
  ) || return

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

  if cwd="$(cat -- "$tmp")" &&
    [[ -n $cwd && $cwd != "$PWD" ]]; then
    cd -- "$cwd"
  fi

  command rm -f -- "$tmp"
}

lfcd() {
  cd "$(command lf -print-last-dir "$@")"
}

bind '"\C-p": history-search-backward'
bind '"\C-n": history-search-forward'

bind '"\C-x\C-e": edit-and-execute-command'

if command -v fzf >/dev/null 2>&1; then
  eval "$(fzf --bash)" 2>/dev/null
fi

export MANPAGER='nvim +Man!'
export ESCDELAY=25
export LESS='-RQKcig -j.5 --incsearch --no-vbell -x4 --use-color -DPw -DEw'
export FZF_DEFAULT_OPTS="--style minimal \
  --info inline-right --color 'bg+:-1,fg+:15,gutter:-1,pointer:4,border:8' \
  --layout=reverse --height 50% --prompt '❯ ' --gutter ' ' \
  --bind 'ctrl-d:preview-half-page-down,ctrl-u:preview-half-page-up,ctrl-e:preview-down'"
