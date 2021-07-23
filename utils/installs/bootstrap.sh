#!/bin/bash

DOTFILES="$HOME/.dotfiles"
TMP="$HOME/tmp"
CWD=$(pwd)

mkdir -p "$TMP"

set -e


function install-nix() {
  print_step "Install Nix."
  cd "$TMP"
  rm -rf nix-install.sh
  curl --proto '=https' --tlsv1.2 -sSf https://nixos.org/nix/install -o nix-install.sh
  chmod +x nix-install.sh
  # ./nix-install.sh --daemon
  rm -rf nix-install.sh
}

function add-unstable-channel() {
  print_step "Add unstable channel."
  nix-channel --add https://nixos.org/channels/nixpkgs-unstable
  nix-channel --update
}

function install-home-manager() {
  print_step "Install Home Manager."
  cd "$DOTFILES"
  stow nix
  nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
  nix-channel --update
  export NIX_PATH=$HOME/.nix-defexpr/channels${NIX_PATH:+:}$NIX_PATH
  nix-shell '<home-manager>' -A install
}

function first-install() {
  install-nix
  add-unstable-channel
  install-home-manager
}
