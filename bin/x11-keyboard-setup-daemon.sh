#!/bin/sh
#
# keyboard-setup-daemon.sh — Apply xset/setxkbmap at startup and on hot‑plug
#

# === CONFIG ===
KEY_REPEAT_DELAY=300
KEY_REPEAT_RATE=50
KEY_LAYOUTS="us,ir"
KEY_OPTIONS="grp:alt_shift_toggle,caps:escape,shift:both_capslock_cancel"

apply_settings() {
    xset r rate "$KEY_REPEAT_DELAY" "$KEY_REPEAT_RATE"
    setxkbmap -layout "$KEY_LAYOUTS" -option "$KEY_OPTIONS"
    echo "[keyboard-setup] Settings applied at $(date)"
}

# Apply once at startup
apply_settings

# Listen for new input devices via udev
udevadm monitor --udev --subsystem-match=input | \
while read -r line; do
    case "$line" in
        *"add"*)
            # Small delay to let X register the device
            sleep 0.5
            apply_settings
            ;;
    esac
done
