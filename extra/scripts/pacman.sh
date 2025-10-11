#!/usr/bin/env bash
set -euo pipefail

echo "==> Configuring pacman..."

# 2. Backup existing config
if [ ! -f /etc/pacman.conf.backup ]; then
  sudo cp /etc/pacman.conf /etc/pacman.conf.backup
fi

# 3. Write minimal reproducible pacman.conf
sudo tee /etc/pacman.conf >/dev/null <<'EOF'
[options]
HoldPkg     = pacman glibc
Architecture = auto
CheckSpace
Color
ILoveCandy
ParallelDownloads = 1
SigLevel    = Required DatabaseOptional
LocalFileSigLevel = Optional
DownloadUser = alpm

[core]
Include = /etc/pacman.d/mirrorlist

[extra]
Include = /etc/pacman.d/mirrorlist
EOF

# 5. Ensure pacman-contrib is installed (for paccache)
if ! command -v paccache >/dev/null 2>&1; then
  sudo pacman -S --noconfirm pacman-contrib
fi

# 6. Clean old packages (keep only 2 versions)
sudo paccache -rk2 || true

echo "==> pacman configured:"
echo "    - Color + ILoveCandy enabled"
echo "    - ParallelDownloads=1"
echo "    - Signature checking enforced"
echo "    - Config reproducible under /etc/pacman.conf"
