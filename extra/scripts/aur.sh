#!/usr/bin/env bash
set -euo pipefail

echo "==> Installing yay (AUR helper)..."

# Install build deps
sudo pacman -S --needed --noconfirm git base-devel

# Clone yay if not already installed
if ! command -v yay &>/dev/null; then
  git clone https://aur.archlinux.org/yay.git /tmp/yay
  (cd /tmp/yay && makepkg -si --noconfirm)
fi

echo "==> yay installed:"
yay --version
