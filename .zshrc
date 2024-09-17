

typeset -U path cdpath fpath manpath

for profile in ${(z)NIX_PROFILES}; do
  fpath+=($profile/share/zsh/site-functions $profile/share/zsh/$ZSH_VERSION/functions $profile/share/zsh/vendor-completions)
done

HELPDIR="/nix/store/y6b258mhrmgn95cl2qzky2fblh9aswrj-zsh-5.9/share/zsh/$ZSH_VERSION/help"

# Use emacs keymap as the default.
bindkey -e



path+="$HOME/.config/zsh/plugins/fzf-tab"
fpath+="$HOME/.config/zsh/plugins/fzf-tab"

# Oh-My-Zsh/Prezto calls compinit during initialization,
# calling it twice causes slight start up slowdown
# as all $fpath entries will be traversed again.
autoload -U compinit && compinit
source /nix/store/p4kk55fjxrfyah3r9c2l3aswagvqxpjg-zsh-autosuggestions-0.7.0/share/zsh-autosuggestions/zsh-autosuggestions.zsh
ZSH_AUTOSUGGEST_STRATEGY=(history)





if [[ -f "$HOME/.config/zsh/plugins/fzf-tab/fzf-tab.plugin.zsh" ]]; then
  source "$HOME/.config/zsh/plugins/fzf-tab/fzf-tab.plugin.zsh"
fi


# History options should be set in .zshrc and after oh-my-zsh sourcing.
# See https://github.com/nix-community/home-manager/issues/177.
HISTSIZE="10000"
SAVEHIST="10000"

HISTFILE="/home/mahdi/.local/share/zsh/zsh_history"
mkdir -p "$(dirname "$HISTFILE")"

setopt HIST_FCNTL_LOCK
unsetopt APPEND_HISTORY
setopt HIST_IGNORE_DUPS
unsetopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_EXPIRE_DUPS_FIRST
setopt SHARE_HISTORY
unsetopt EXTENDED_HISTORY
setopt autocd

if [[ $options[zle] = on ]]; then
  eval "$(/nix/store/2f27qfzb3b3s09rjfz0g06g03igibnx1-fzf-0.55.0/bin/fzf --zsh)"
fi

export PATH="$HOME/.dotfiles/bin:$PATH"
export MANPAGER='nvim +Man!'

# Disable C-s freezing the terminal
stty -ixon

# C-Backspace for word deletions
bindkey "^H" backward-kill-word

bindkey '^p' history-search-backward
bindkey '^n' history-search-forward

# open commands in $EDITOR with C-x C-e
autoload -z edit-command-line
zle -N edit-command-line
bindkey "^X^E" edit-command-line

# Completion styling
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'
zstyle ':completion:*' list-colors "$${(s.:.)LS_COLORS}"
zstyle ':completion:*' menu no
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'eza -1 --color=always $realpath'
zstyle ':completion:*:descriptions' format '[%d]'
zstyle ':fzf-tab:*' switch-group '<' '>'

autoload -U select-word-style
select-word-style bash

# [[ ! -r /home/mahdi/.opam/opam-init/init.zsh ]] || source /home/mahdi/.opam/opam-init/init.zsh  > /dev/null 2> /dev/null

function ya() {
  local tmp="$(mktemp -t "yazi-cwd.XXXXX")"
  yazi "$@" --cwd-file="$tmp"
  if cwd="$(cat -- "$tmp")" && [ -n "$cwd" ] && [ "$cwd" != "$PWD" ]; then
    cd "$cwd"
  fi
  rm -f -- "$tmp"
}

GPG_TTY="$(tty)"
export GPG_TTY

eval "$(/nix/store/86ag65bw34mz1yi5b2llsdm7kh3561z5-zoxide-0.9.4/bin/zoxide init zsh --cmd cd)"

if [[ $TERM != "dumb" ]]; then
  eval "$(/home/mahdi/.nix-profile/bin/starship init zsh)"
fi

source /nix/store/bgy6dk7p0694fh4vyvlmpqwq2ybph749-nix-index-with-db-0.1.8/etc/profile.d/command-not-found.sh

if test -n "$KITTY_INSTALLATION_DIR"; then
  export KITTY_SHELL_INTEGRATION="no-rc"
  autoload -Uz -- "$KITTY_INSTALLATION_DIR"/shell-integration/zsh/kitty-integration
  kitty-integration
  unfunction kitty-integration
fi

eval "$(/nix/store/823qi3acdfxnvn0ylygjg6jabx7b4q31-direnv-2.34.0/bin/direnv hook zsh)"


# Aliases
alias -- ..='cd ..'
alias -- ...='cd ../..'
alias -- ....='cd ../../..'
alias -- c=clear
alias -- cp='cp -iv'
alias -- dadjoke='/nix/store/fccd0wdqxp93lfywary5jxpsg1n0rqp3-curl-8.9.1-bin/bin/curl --header "Accept: text/plain" https://icanhazdadjoke.com/ && echo'
alias -- egrep='egrep --color=auto'
alias -- eza='eza --icons --group-directories-first'
alias -- fgrep='fgrep --color=auto'
alias -- grep='grep --color=auto'
alias -- l='eza -la --icons --no-user --no-permissions --no-filesize --no-time --group-directories-first'
alias -- la='eza -a'
alias -- lg=lazygit
alias -- ll='eza -l'
alias -- lla='eza -la'
alias -- ls='eza --group-directories-first'
alias -- lt='eza --tree'
alias -- mv='mv -iv'
alias -- rm='rm -vI'
alias -- ta='tmux attach'
alias -- tc=tmux-sessionizer
alias -- tree='eza --tree --icons --group-directories-first'
alias -- xdg-open=/nix/store/qrlzxcns49wy5wk2ssgb0a872kh9v45w-mimeo-2023/bin/mimeo

# Named Directory Hashes


source /nix/store/snk5f9agv9dwvda8k03pmsw9aqzfziff-zsh-syntax-highlighting-0.8.0/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
ZSH_HIGHLIGHT_HIGHLIGHTERS+=()




