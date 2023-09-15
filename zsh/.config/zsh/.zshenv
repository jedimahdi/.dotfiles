#!/usr/bin/env zsh

###############################
# EXPORT ENVIRONMENT VARIABLE #
###############################

export DOTFILES="$HOME/.dotfiles"
export CACHEDIR="$HOME/.local/share"
export VIM_TMP="$HOME/.vim-tmp"
export ZSH="$HOME/.config/zsh"

[[ -d "$CACHEDIR" ]] || mkdir -p "$CACHEDIR"
[[ -d "$VIM_TMP" ]] || mkdir -p "$VIM_TMP"

# XDG
export XDG_CONFIG_HOME=$HOME/.config
export XDG_DATA_HOME=$HOME/.local/share
export XDG_CACHE_HOME=$HOME/.cache

# editor
export EDITOR="nvim"
export VISUAL="nvim"

# zsh
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
export HISTFILE="$ZDOTDIR/.zhistory"    # History filepath
export HISTSIZE=10000                   # Maximum events for internal history
export SAVEHIST=10000                   # Maximum events in history file

# other software
export VIMCONFIG="$XDG_CONFIG_HOME/nvim"
export SCREENSHOT="$HOME/Documents/images/screenshots"

# Man pages
export MANPAGER='nvim +Man!'


# fzf
export FZF_DEFAULT_COMMAND='rg --files --hidden --glob "!.git"'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
FZF_COLORS="bg+:-1,\
fg:gray,\
fg+:white,\
border:black,\
spinner:0,\
hl:yellow,\
header:blue,\
info:green,\
pointer:red,\
marker:blue,\
prompt:gray,\
hl+:red"
export FZF_DEFAULT_OPTS="--height 60% \
--layout reverse \
--color '$FZF_COLORS' \
--prompt '∷ ' \
--info=hidden"
export FZF_ALT_C_OPTS="--preview 'tree -C {} | head -n 10'"
export FZF_COMPLETION_DIR_COMMANDS="cd pushd rmdir tree ls"

# NPM
export NPM_PATH="$HOME/.npm-global"
export NPM_BIN="$HOME/.npm-global/bin"
export BUN_INSTALL="$HOME/.bun"

# PATH
export PATH="$NPM_BIN:$PATH"                                        # NPM
export PATH="$DOTFILES/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.cabal/bin:$PATH"
export PATH="$HOME/.ghcup/bin:$PATH"
export PATH="$HOME/go/bin:$PATH"
export PATH="$HOME/.pack/bin:$PATH"
export PATH="$BUN_INSTALL/bin:$PATH"
# export NIX_PATH=$HOME/.nix-defexpr/channels${NIX_PATH:+:}$NIX_PATH

# fpath=(
#     $ZSH/functions
#     /usr/local/share/zsh/site-functions
#     $fpath
# )
