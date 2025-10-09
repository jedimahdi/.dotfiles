#!/usr/bin/env bash
set -euo pipefail

echo "==> Configuring minimal audio stack (PipeWire + WirePlumber)..."

# 1. Remove legacy/conflicting audio daemons if present
for pkg in pulseaudio pulseaudio-alsa pulseaudio-bluetooth pulseaudio-jack jack2; do
  if pacman -Qq "$pkg" &>/dev/null; then
    sudo pacman -Rns --noconfirm "$pkg"
  fi
done

# 2. Install PipeWire stack
sudo pacman -S --needed --noconfirm pipewire pipewire-alsa pipewire-pulse pipewire-jack wireplumber

# 3. Enable services (idempotent)
systemctl --user enable --now pipewire.service
systemctl --user enable --now pipewire-pulse.service
systemctl --user enable --now wireplumber.service

echo "==> PipeWire audio stack installed and enabled."
echo "    - PulseAudio replaced by pipewire-pulse"
echo "    - JACK replaced by pipewire-jack"
echo "    - Session manager: WirePlumber"
