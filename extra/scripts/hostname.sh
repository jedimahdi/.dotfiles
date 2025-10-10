#!/usr/bin/env bash
set -euo pipefail

HOSTNAME="mahdi-pc"

echo "==> Setting hostname to $HOSTNAME..."
sudo hostnamectl set-hostname "$HOSTNAME"

sudo tee /etc/hosts >/dev/null <<EOF
127.0.0.1   localhost
::1         localhost ip6-localhost ip6-loopback
127.0.1.1   $HOSTNAME.localdomain $HOSTNAME
EOF

echo "==> Hostname configured:"
hostnamectl
