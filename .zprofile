export BROWSER="firefox"
export PAGER="less"
export EDITOR="nvim"
export VISUAL="nvim"
export TERMINAL="alacritty"

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_STATE_HOME="$HOME/.local/state"

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

export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/neovim/bin:$PATH"
export PATH="$HOME/.dotfiles/bin:$PATH"
export PATH="$CARGO_HOME/bin:$PATH"
export PATH="$GOBIN:$PATH"
export PATH="$HOME/.npm-global/bin:$PATH"

export MANPAGER='nvim +Man!'
# export MANWIDTH=999
export ESCDELAY=25
export LESS='-FRQKcig -j.5 --incsearch --no-vbell -x4 --use-color -DPw'

export FZF_DEFAULT_OPTS="--style minimal --info inline-right --color 'bg+:-1,fg+:15,gutter:-1,pointer:4,border:8' --layout=reverse --height 50% --prompt '❯ ' --gutter ' '"
# export FZF_CTRL_R_OPTS="--no-preview"
