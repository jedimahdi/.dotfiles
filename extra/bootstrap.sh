#!/bin/bash

set -euo pipefail

DOTFILES="$HOME/.dotfiles"

echo "[*] Setting up XDG base dirs..."
mkdir -p ~/.config ~/.local/share ~/.local/state ~/.local/bin ~/.cache

echo "[*] Disabling unused services..."
# sudo systemctl disable --now bluetooth.service || true
sudo systemctl disable --now sshd.service || true
systemctl --user mask xdg-user-dirs-update.service xdg-user-dirs-update.timer || true

echo "[*] Cleaning up autostart entries..."
mkdir -p ~/.config/autostart
for f in at-spi-dbus-bus.desktop xdg-user-dirs.desktop; do
  if [ -f /etc/xdg/autostart/$f ]; then
    cp /etc/xdg/autostart/$f ~/.config/autostart/
    echo "Hidden=true" >>~/.config/autostart/$f
  fi
done

systemctl --user enable --now foot-server.service mako.service hyprpaper.service

source $DOTFILES/extra/scripts/network.sh
source $DOTFILES/extra/scripts/time.sh
source $DOTFILES/extra/scripts/audio.sh
