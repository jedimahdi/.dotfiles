#!/bin/bash

source "$DOTFILES/utils/installs/nix.sh"
source "$DOTFILES/utils/installs/node.sh"
source "$DOTFILES/utils/installs/zsh.sh"

mkdir -p "$TMP"

set -e

function first-install() {
  install-nix-full
  install-zsh-full
}
