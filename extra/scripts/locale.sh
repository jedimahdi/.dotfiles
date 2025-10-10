#!/usr/bin/env bash
set -euo pipefail

echo "==> Configuring minimal locale and keyboard settings..."

# 1. Remove legacy/conflicting locale tools if present
# (Most systems won’t have these, but loop keeps it idempotent)
for pkg in console-setup; do
  if pacman -Qq "$pkg" &>/dev/null; then
    sudo pacman -Rns --noconfirm "$pkg"
  fi
done

# 2. Ensure glibc (provides locale data) is installed
sudo pacman -S --needed --noconfirm glibc

# 3. Generate desired locale(s)
# Example: en_US.UTF-8 and fa_IR.UTF-8
sudo sed -i 's/^#en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen
sudo sed -i 's/^#fa_IR.UTF-8 UTF-8/fa_IR.UTF-8 UTF-8/' /etc/locale.gen
sudo locale-gen

# 4. Set system locale (idempotent)
sudo localectl set-locale LANG=en_US.UTF-8

# Console (TTY)
sudo localectl set-keymap us

# X11 (graphical)
sudo localectl set-x11-keymap us

echo "==> Locale configured:"
localectl status
