#!/usr/bin/env bash
set -euo pipefail

echo "==> Configuring minimal network stack (iwd + systemd-networkd + systemd-resolved)..."

# 1. Remove legacy/conflicting packages
# - wpa_supplicant: replaced by iwd
# - dhcpcd, netctl: deprecated, replaced by systemd-networkd
# - NetworkManager, ConnMan: full network managers, not needed in minimal setup
for pkg in dhcpcd netctl network-manager-applet networkmanager connman wpa_supplicant; do
  if pacman -Qq "$pkg" &>/dev/null; then
    sudo pacman -Rns --noconfirm "$pkg"
  fi
done

# 2. Install required packages
sudo pacman -S --needed --noconfirm iwd systemd

# 3. Enable services (idempotent: enabling twice is safe)
sudo systemctl enable --now iwd.service
sudo systemctl enable --now systemd-networkd.service
sudo systemctl enable --now systemd-resolved.service

# 4. Configure global DNS in /etc/systemd/resolved.conf
#    Enforces Cloudflare + Quad9 + Google, disables fallback
sudo mkdir -p /etc/systemd
sudo tee /etc/systemd/resolved.conf >/dev/null <<'EOF'
[Resolve]
DNS=1.1.1.1 9.9.9.9 8.8.8.8
FallbackDNS=
DNSStubListener=yes
EOF

# 5. Create .network templates for wired + wireless
#    These ensure DHCP is used for IP, but DNS from DHCP is ignored
sudo mkdir -p /etc/systemd/network

# Wired (match eno* or enp*)
sudo tee /etc/systemd/network/20-wired.network >/dev/null <<'EOF'
[Match]
Name=en*

[Network]
DHCP=yes

[DHCPv4]
UseDNS=no

[DHCPv6]
UseDNS=no
EOF

# Wireless (match wlan*)
sudo tee /etc/systemd/network/20-wlan.network >/dev/null <<'EOF'
[Match]
Name=wlan*

[Network]
DHCP=yes

[DHCPv4]
UseDNS=no

[DHCPv6]
UseDNS=no
EOF

# 6. Ensure resolv.conf points to stub resolver
if [ ! -L /etc/resolv.conf ] || [ "$(readlink -f /etc/resolv.conf)" != "/run/systemd/resolve/stub-resolv.conf" ]; then
  sudo mv /etc/resolv.conf /etc/resolv.conf.backup.$(date +%s) || true
  sudo ln -sf /run/systemd/resolve/stub-resolv.conf /etc/resolv.conf
fi

# 7. Restart services to apply changes
sudo systemctl restart systemd-networkd
sudo systemctl restart systemd-resolved

# 8. Print info
if command -v iwctl >/dev/null; then
  echo
  echo "-- iwctl devices --"
  iwctl device list || true
fi

# 4. Show DNS configuration
echo
echo "-- resolvectl status --"
resolvectl status

echo "==> Network stack configured:"
echo "    - Wi-Fi: iwd (replaces wpa_supplicant)"
echo "    - Wired/Wi-Fi config: systemd-networkd (replaces dhcpcd/netctl)"
echo "    - DNS: systemd-resolved with Cloudflare + Quad9 + Google"
echo "    - Router DNS ignored (UseDNS=no)"
