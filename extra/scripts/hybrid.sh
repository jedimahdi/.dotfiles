#!/usr/bin/env bash
set -euo pipefail

echo "==> Configuring hybrid Intel + AMD graphics stack..."

# 1. Remove conflicting/legacy drivers
# - libva-intel-driver (i965) is legacy, not needed for UHD 620
# - xf86-video-intel is deprecated; modesetting driver in Xorg is preferred
# - old catalyst/fglrx (if somehow present) conflicts with Mesa
for pkg in libva-intel-driver xf86-video-intel catalyst; do
  if pacman -Qq "$pkg" &>/dev/null; then
    echo "Removing conflicting package: $pkg"
    sudo pacman -Rns --noconfirm "$pkg"
  fi
done

# 2. Core graphics libraries
sudo pacman -S --needed --noconfirm \
  mesa libglvnd \
  vulkan-icd-loader vulkan-tools \
  mesa-demos

# 3. Intel UHD 620 (Kaby Lake Refresh)
echo "==> Installing Intel drivers..."
sudo pacman -S --needed --noconfirm \
  vulkan-intel intel-media-driver

# 4. AMD Radeon R7 M340 (Iceland, GCN 1.0/1.1)
echo "==> Installing AMD drivers..."
sudo pacman -S --needed --noconfirm \
  vulkan-radeon libva-mesa-driver

# 5. Hybrid PRIME offloading
echo "==> Hybrid setup ready."
echo "Intel is default (power saving)."
echo "Run apps on AMD with: DRI_PRIME=1 <command>"

# 6. Quick verification hints
echo "==> To verify:"
echo "  glxinfo | grep 'OpenGL renderer'"
echo "  vulkaninfo | grep 'deviceName'"
echo "  DRI_PRIME=1 glxinfo | grep 'OpenGL renderer'   # should show AMD"
