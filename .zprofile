export BROWSER="firefox"
export PAGER="less"
export EDITOR="nvim"
export TERMINAL="alacritty"
export VISUAL="${EDITOR}"

# follow XDG base dir specification
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"

# bootstrap .zshrc to ~/.config/zsh/.zshrc, any other zsh config files can also reside here
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"

# history files
export LESSHISTFILE="$XDG_CACHE_HOME/less_history"
export PYTHON_HISTORY="$XDG_DATA_HOME/python/history"

export GOPATH="$XDG_DATA_HOME/go"
export GOBIN="$GOPATH/bin"
export GOMODCACHE="$XDG_CACHE_HOME/go/mod"
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export FFMPEG_DATADIR="$XDG_CONFIG_HOME/ffmpeg"
export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc-2.0" # gtk 3 & 4 are XDG compliant
export PASSWORD_STORE_DIR="$XDG_DATA_HOME/pass"
export GNUPGHOME="$XDG_DATA_HOME/gnupg"
export RIPGREP_CONFIG_PATH="$XDG_CONFIG_HOME/ripgrep/config"

export PATH="$HOME/.dotfiles/bin:$PATH"
export PATH="$CARGO_HOME/bin:$PATH"
export PATH="$GOBIN:$PATH"
export PATH="$HOME/.npm-global/bin:$PATH"

export FZF_DEFAULT_OPTS="--style minimal --info inline-right --color 'bg+:-1,fg+:15,gutter:-1,pointer:4,border:8' --border rounded --layout=reverse --height 80% --tmux 90% --prompt '❯ '"
export FZF_CTRL_R_OPTS="--no-preview"
export MANPAGER='nvim +Man!'
export MANWIDTH=999

export LESS='-cigRS -j4 -x4 -#5 -z-10'
export LESSPROMPT='?f%f .?ltLine %lt:?pt%pt\%:?btByte %bt:-...'

# Settings: LS_COLORS
LS_COLORS='no=0:fi=0:di=34'

# Hidden files
# LS_COLORS+=":.*=90"

# Programming (purple)
LS_COLORS+=":*.py=36:*.sh=36:*.c=36:*.h=36"
LS_COLORS+=":*Dockerfile=36:*Makefile=36"

# Text files (green)
# LS_COLORS+=":*.md=32:*.txt=32:*.html=32"
LS_COLORS+=":ex=32"

# Config files (yellow)
LS_COLORS+=":*.json=33:*.toml=33:*.yml=33"
LS_COLORS+=":*.in=33:*.conf=33:*.example=33"
LS_COLORS+=":.zshrc=33:.zprofile=33"
export LS_COLORS

if uwsm check may-start; then
  exec uwsm start hyprland-uwsm.desktop
fi
