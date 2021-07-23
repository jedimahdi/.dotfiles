#!/bin/bash

source "$DOTFILES/utils/installs/shared.sh"

function install-zsh() {
  sudo pacman -S zsh --needed --noconfirm
  mkdir -p ~/.cache/zsh
  chsh -s $(which zsh)
  make_symlink zsh
  # cd "$DOTFILES"
  # stow zsh
}

function install-fzf() {
  git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
  ~/.fzf/install
}
