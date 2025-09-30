#!/bin/bash

set -euo pipefail

echo "[*] Setting up XDG base dirs..."
mkdir -p ~/.config ~/.local/share ~/.local/state ~/.cache

systemctl --user mask xdg-user-dirs-update.service || true
