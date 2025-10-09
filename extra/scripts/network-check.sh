#!/usr/bin/env bash
set -euo pipefail

echo "==> Network stack sanity check"

# 1. Show enabled services
echo
echo "-- Services --"
systemctl is-enabled iwd.service systemd-networkd.service systemd-resolved.service || true
systemctl --user is-enabled iwd.service systemd-networkd.service systemd-resolved.service 2>/dev/null || true

# 2. Show link status
echo
echo "-- networkctl --"
networkctl list

# 3. Show Wi-Fi devices (if any)
if command -v iwctl >/dev/null; then
  echo
  echo "-- iwctl devices --"
  iwctl device list || true
fi

# 4. Show DNS configuration
echo
echo "-- resolvectl status --"
resolvectl status

# 5. Confirm /etc/resolv.conf points to stub
echo
echo "-- resolv.conf --"
ls -l /etc/resolv.conf
head -n 5 /etc/resolv.conf

# 6. Test DNS resolution through stub
echo
echo "-- DNS test (dig via stub) --"
if command -v dig >/dev/null; then
  dig @127.0.0.53 archlinux.org +short || true
else
  getent hosts archlinux.org || true
fi

# 7. Query resolved directly
echo
echo "-- resolvectl query --"
resolvectl query archlinux.org || true

echo
echo "==> Network check complete."
