#!/usr/bin/env bash
set -euo pipefail

echo "==> Configuring minimal power/laptop management (systemd-logind only)..."

# 1. Remove legacy/conflicting daemons if present
for pkg in acpid tlp laptop-mode-tools tuned; do
  if pacman -Qq "$pkg" &>/dev/null; then
    sudo pacman -Rns --noconfirm "$pkg"
  fi
done

# 2. No need for upower (battery info) since your battery is dead
#    No need for tlp (power tuning) since systemd covers suspend/hibernate

# 3. Configure logind for lid/power button behavior
sudo tee /etc/systemd/logind.conf >/dev/null <<'EOF'
[Login]
# Suspend when laptop lid is closed
HandleLidSwitch=suspend
HandleLidSwitchExternalPower=suspend
HandleLidSwitchDocked=ignore

# Power button shuts down
HandlePowerKey=poweroff

# Hibernate key hibernates
HandleHibernateKey=hibernate

# Suspend key suspends
HandleSuspendKey=suspend
EOF

# 4. Restart logind to apply changes
sudo systemctl restart systemd-logind

echo "==> Power management configured:"
echo "    - acpid/tlp removed"
echo "    - systemd-logind handles lid/power/suspend/hibernate"
echo "    - No battery daemons installed (since battery is dead)"
