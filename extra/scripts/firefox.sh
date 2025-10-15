#!/usr/bin/env bash
set -euo pipefail

sudo pacman -S --needed --noconfirm firefox

DOTFILES="$HOME/.dotfiles"
PROFILE_DIR="${HOME}/.mozilla/firefox"
USER_JS="user.js"

# Find default profile (the one with 'default-release')
PROFILE=$(grep 'Path=' "${PROFILE_DIR}/profiles.ini" |
  grep default-release |
  cut -d= -f2- ||
  true)

if [[ -z "$PROFILE" ]]; then
  echo "No default-release profile found. Please start Firefox once first."
  exit 1
fi

PROFILE_PATH="${PROFILE_DIR}/${PROFILE}"

cp -f "${DOTFILES}/extra/firefox/${USER_JS}" "${PROFILE_PATH}/${USER_JS}"
echo "✔ user.js copied to: ${PROFILE_PATH}/${USER_JS}"
