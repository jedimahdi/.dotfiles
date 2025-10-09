#!/usr/bin/env bash
set -euo pipefail

echo "==> Configuring minimal logging stack (systemd-journald)..."

# 1. Remove legacy/conflicting syslog daemons if present
for pkg in syslog-ng rsyslog metalog sysklogd; do
  if pacman -Qq "$pkg" &>/dev/null; then
    sudo pacman -Rns --noconfirm "$pkg"
  fi
done

# 2. Configure persistent journald
sudo mkdir -p /var/log/journal
sudo tee /etc/systemd/journald.conf >/dev/null <<'EOF'
[Journal]
Storage=persistent
Compress=yes
SystemMaxUse=200M
RuntimeMaxUse=50M
EOF

# 3. Restart journald to apply config
sudo systemctl restart systemd-journald

echo "==> Logging stack configured:"
echo "    - Only systemd-journald active"
echo "    - Persistent logs under /var/log/journal"
echo "    - Disk usage capped (200M persistent, 50M runtime)"
