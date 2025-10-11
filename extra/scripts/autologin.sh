#!/usr/bin/env bash
set -euo pipefail

USER_NAME="${1:-$USER}"   # default to current user if not passed

echo "[*] Setting up agetty autologin for user: $USER_NAME"

# Create override directory
sudo mkdir -p /etc/systemd/system/getty@tty1.service.d

# Write override.conf atomically (idempotent: overwrites if exists)
sudo tee /etc/systemd/system/getty@tty1.service.d/autologin.conf >/dev/null <<EOF
[Service]
ExecStart=
ExecStart=-/sbin/agetty --noreset --noclear --autologin ${USER_NAME} - %I \$TERM
EOF

# Reload systemd so it picks up the override
sudo systemctl daemon-reexec

echo "[*] Autologin configured for $USER_NAME on tty1. Reboot to test."
