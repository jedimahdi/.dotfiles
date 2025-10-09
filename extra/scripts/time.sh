#!/usr/bin/env bash
set -euo pipefail

echo "==> Configuring minimal time synchronization (systemd-timesyncd)..."

# 1. Remove legacy/conflicting NTP daemons if present
for pkg in ntp openntpd chrony; do
  if pacman -Qq "$pkg" &>/dev/null; then
    sudo pacman -Rns --noconfirm "$pkg"
  fi
done

# 2. Ensure systemd (with timesyncd) is installed
sudo pacman -S --needed --noconfirm systemd

# 3. Configure timesyncd with Arch pool servers
sudo mkdir -p /etc/systemd
sudo tee /etc/systemd/timesyncd.conf >/dev/null <<'EOF'
[Time]
NTP=0.arch.pool.ntp.org 1.arch.pool.ntp.org 2.arch.pool.ntp.org 3.arch.pool.ntp.org
FallbackNTP=0.pool.ntp.org 1.pool.ntp.org
EOF

# 4. Enable NTP sync (idempotent)
sudo timedatectl set-ntp true

echo "==> Time sync configured:"
timedatectl status | grep -E "System clock|NTP"

# 5. Configure timezone
# Replace with your preferred timezone, e.g. Asia/Tehran
if ! timedatectl show | grep -q "Timezone=Asia/Tehran"; then
  sudo timedatectl set-timezone Asia/Tehran
fi
