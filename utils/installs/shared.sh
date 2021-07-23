#!/bin/bash

DOTFILES="$HOME/.dotfiles"
TMP="$HOME/tmp"
CWD=$(pwd)

function print_step() {
  GREEN='\033[0;32m'
  RESET='\033[0m' # No Color
  printf "$GREEN>>>>>> $1$RESET\n"
}

function make_symlink() {
  stow -d "$DOTFILES" -t "$HOME" "$1"
}
