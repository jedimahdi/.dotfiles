#!/usr/bin/env bash
set -euo pipefail

echo "==> Updating pacman mirrorlist with reflector..."

# 1. Install reflector if missing
sudo pacman -S --needed --noconfirm reflector

# 2. Backup current mirrorlist once
if [ ! -f /etc/pacman.d/mirrorlist.backup ]; then
  sudo cp /etc/pacman.d/mirrorlist /etc/pacman.d/mirrorlist.backup
fi

# 3. Generate a new mirrorlist
# -c Netherlands: prefer Dutch mirrors
# -c Germany: add a reliable fallback region
# -p https: only https mirrors
# -l 20: top 20 mirrors
# --sort rate: fastest first
sudo reflector --verbose \
  -c Netherlands -c Germany \
  -p https \
  -l 20 \
  --sort rate \
  --save /etc/pacman.d/mirrorlist

# 4. Refresh package databases
sudo pacman -Sy --noconfirm

echo "==> Mirrorlist updated:"
head -n 10 /etc/pacman.d/mirrorlist
