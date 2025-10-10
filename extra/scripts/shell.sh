#!/usr/bin/env bash
set -euo pipefail

# Ensure zsh is installed
if ! command -v zsh >/dev/null; then
  sudo pacman -S --needed zsh fzf tmux
fi

# Change default shell
if [ "$SHELL" != "$(command -v zsh)" ]; then
  chsh -s "$(command -v zsh)"
  echo "Default shell changed to zsh. You may need to log out and back in."
fi
