#!/bin/bash

source "$DOTFILES/utils/installs/shared.sh"

function install-npm() {
  sudo pacman -S npm --needed --noconfirm
  mkdir -p ~/.npm-global
  npm config set prefix '~/.npm-global'
}

function install-ts() {
  npm install -g typescript typescript-language-server
}

