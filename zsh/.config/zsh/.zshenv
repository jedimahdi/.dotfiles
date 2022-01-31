export DOTFILES="$HOME/.dotfiles"
export CACHEDIR="$HOME/.local/share"
export VIM_TMP="$HOME/.vim-tmp"
export ZSH="$HOME/.config/zsh"

[[ -d "$CACHEDIR" ]] || mkdir -p "$CACHEDIR"
[[ -d "$VIM_TMP" ]] || mkdir -p "$VIM_TMP"

[[ -f ~/.zshenv.local ]] && source ~/.zshenv.local

fpath=(
    $ZSH/functions
    /usr/local/share/zsh/site-functions
    $fpath
)

typeset -aU path

export EDITOR='nvim'
export GIT_EDITOR='nvim'

# . "$HOME/.cargo/env"
