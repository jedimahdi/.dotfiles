#!/usr/bin/env bash
set -euo pipefail

# --- CONFIGURATION ---
# Replace with your actual interface names (check with `ip link`)
ETH_IFACE="eno1"
WIFI_IFACE="wlan0"

# --- INSTALL PACKAGES ---
echo "[*] Installing iwd and systemd components..."
sudo pacman -Syu --noconfirm iwd systemd systemd-resolvconf

# --- DISABLE NETWORKMANAGER IF PRESENT ---
if systemctl is-enabled --quiet NetworkManager 2>/dev/null; then
  echo "[*] Disabling NetworkManager..."
  sudo systemctl disable --now NetworkManager
  sudo systemctl mask NetworkManager
fi

# --- ENABLE CORE SERVICES ---
echo "[*] Enabling iwd, systemd-networkd, and systemd-resolved..."
sudo systemctl enable --now iwd.service
sudo systemctl enable --now systemd-networkd.service
sudo systemctl enable --now systemd-resolved.service

# --- FIX RESOLV.CONF ---
echo "[*] Linking resolv.conf to systemd-resolved..."
sudo rm -f /etc/resolv.conf
sudo ln -s /run/systemd/resolve/resolv.conf /etc/resolv.conf

# --- CREATE NETWORK FILES ---
echo "[*] Creating systemd-networkd configs..."

# Ethernet DHCP
sudo tee /etc/systemd/network/20-wired.network >/dev/null <<EOF
[Match]
Name=${ETH_IFACE}

[Network]
DHCP=yes
EOF

# Wi-Fi DHCP
sudo tee /etc/systemd/network/25-wireless.network >/dev/null <<EOF
[Match]
Name=${WIFI_IFACE}

[Network]
DHCP=yes
EOF

# --- DONE ---
echo "[*] Setup complete!"
echo "Use 'iwctl' to connect to Wi-Fi:"
echo "    iwctl station ${WIFI_IFACE} scan"
echo "    iwctl station ${WIFI_IFACE} get-networks"
echo "    iwctl station ${WIFI_IFACE} connect <SSID>"
echo
echo "Verify with:"
echo "    networkctl status"
echo "    resolvectl status"
