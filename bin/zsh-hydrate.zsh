#!/usr/bin/env zsh

ZSHARE="${XDG_DATA_HOME:-$HOME/.local/share}/zsh"
PLUGINS_DIR="$ZSHARE/plugins"

# List of plugins: name and git URL
typeset -A plugins
plugins=(
  zsh-autosuggestions https://github.com/zsh-users/zsh-autosuggestions.git
  zsh-syntax-highlighting https://github.com/zsh-users/zsh-syntax-highlighting.git
)

# Ensure plugins directory exists
mkdir -p "$PLUGINS_DIR"

echo "==> Checking plugins..."
for name url in ${(kv)plugins}; do
  plugin_dir="$PLUGINS_DIR/$name"
  if [[ ! -d "$plugin_dir/.git" ]]; then
    echo "Cloning $name..."
    git clone --depth=1 "$url" "$plugin_dir"
  else
    echo "Updating $name..."
    git -C "$plugin_dir" pull --ff-only
  fi

  # Compile all .zsh files in the plugin directory to .zwc
  for f in "$plugin_dir"/*.zsh(N); do
    if [[ -f "$f" ]]; then
      zwc_file="${f}.zwc"
      echo "Compiling $f -> $zwc_file"
      zcompile "$f"
    fi
  done
done

# Regenerate .zcompdump
ZCOMP_DUMP="$ZSHARE/.zcompdump"
echo "==> Regenerating $ZCOMP_DUMP..."
autoload -Uz compinit
rm -f "$ZCOMP_DUMP" "$ZCOMP_DUMP.zwc"
compinit -C -d "$ZCOMP_DUMP"

# Compile .zcompdump for faster loading
if [[ -f "$ZCOMP_DUMP" ]]; then
  echo "==> Compiling $ZCOMP_DUMP.zwc..."
  zcompile "$ZCOMP_DUMP"
fi

echo "==> Done!"
