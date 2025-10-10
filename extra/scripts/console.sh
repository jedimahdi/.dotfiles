#!/usr/bin/env bash
set -euo pipefail

echo "==> Configuring console keymap and font..."

# Example: US keymap + Terminus font
sudo tee /etc/vconsole.conf >/dev/null <<'EOF'
KEYMAP=us
FONT=ter-124n
EOF

# Ensure font package is installed
sudo pacman -S --needed --noconfirm terminus-font

echo "==> Console configured:"
cat /etc/vconsole.conf
