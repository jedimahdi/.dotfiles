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

export PATH="$HOME/.dotfiles/bin:$PATH"
export PATH="$CARGO_HOME/bin:$PATH"
export PATH="$GOBIN:$PATH"
export PATH="$HOME/.npm-global/bin:$PATH"
export PATH="$HOME/.nvim/bin:$PATH"

export FZF_DEFAULT_OPTS="--style minimal --color 16 --layout=reverse --height 30%"
export FZF_CTRL_R_OPTS="--style minimal --color 16 --info inline --no-preview"
export MANPAGER='nvim +Man!'
export MANWIDTH=999

# Settings: less
export LESS='-cigRS -j4 -x4 -#5 -z-10 -Ps'
