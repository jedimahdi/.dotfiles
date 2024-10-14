export ZCONFIG="$HOME/.config/zsh"
export ZSHARE="$HOME/.local/share/zsh"
export EDITOR="nvim"

bindkey -e

autoload -U compinit && compinit

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
alias ls='eza --icons --group-directories-first'
alias mv='mv -iv'
alias rm='rm -vI'
alias cp='cp -iv'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
alias grep='grep --color=auto'
alias lg='lazygit'
alias ta='tmux attach'
alias tc='tmux-sessionizer'
alias l='eza -la --icons --no-user --no-time --group-directories-first'
alias pacman='sudo pacman --color auto'
alias ef="rg --files --hidden -g '!node_modules/' -g '!.git/' -g '!target/' | fzf | xargs nvim"

export PATH="$HOME/.dotfiles/bin:$PATH"
export MANPAGER='nvim +Man!'

bindkey '^p' history-search-backward
bindkey '^n' history-search-forward

# Disable C-s freezing the terminal
stty -ixon

# open commands in $EDITOR with C-x C-e
autoload -z edit-command-line
zle -N edit-command-line
bindkey "^X^E" edit-command-line

source <(fzf --zsh)
eval "$(starship init zsh)"
eval "$(zoxide init zsh --cmd cd)"

function y() {
	local tmp="$(mktemp -t "yazi-cwd.XXXXXX")"
	yazi "$@" --cwd-file="$tmp"
	if cwd="$(cat -- "$tmp")" && [ -n "$cwd" ] && [ "$cwd" != "$PWD" ]; then
		builtin cd -- "$cwd"
	fi
	rm -f -- "$tmp"
}
