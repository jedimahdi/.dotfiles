# zmodload zsh/zprof
export ZSHARE="$XDG_DATA_HOME/zsh"

bindkey -e

autoload -Uz compinit
compinit

HISTFILE="$ZSHARE/zsh_history"
HISTSIZE="10000"
SAVEHIST="10000"

setopt SHARE_HISTORY
setopt HIST_EXPIRE_DUPS_FIRST

source "$ZSHARE/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh"
source "$ZSHARE/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'

alias c='clear'

alias ls='eza --all --icons --group-directories-first'
alias l='eza --all --long --group-directories-first --color=always --icons --octal-permissions --no-time --no-user --no-permissions'
alias la='eza --all --long --group-directories-first --color=always --icons --octal-permissions --time-style="+%Y-%m-%d %H:%M"'

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

bindkey '^p' history-search-backward
bindkey '^n' history-search-forward

# Disable C-s freezing the terminal
stty -ixon

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

if [[ -z "${SSH_CONNECTION}" ]]; then
    export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
fi

source <(fzf --zsh)
eval "$(starship init zsh)"
eval "$(zoxide init zsh --cmd cd)"
# zprof
