export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_STATE_HOME="$HOME/.local/state"

export BROWSER="firefox"
export PAGER="less"
export EDITOR="nvim"
export VISUAL="nvim"
export TERMINAL="alacritty"

export LESSHISTFILE="$XDG_CACHE_HOME/less_history"
export PYTHON_HISTORY="$XDG_DATA_HOME/python/history"

export GOPATH="$XDG_DATA_HOME/go"
export GOBIN="$GOPATH/bin"
export GOMODCACHE="$XDG_CACHE_HOME/go/mod"
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export FFMPEG_DATADIR="$XDG_CONFIG_HOME/ffmpeg"
export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc-2.0"
export PASSWORD_STORE_DIR="$XDG_DATA_HOME/pass"
export GNUPGHOME="$XDG_DATA_HOME/gnupg"
export RIPGREP_CONFIG_PATH="$XDG_CONFIG_HOME/ripgrep/config"

typeset -U path PATH
path=(
  $HOME/.local/bin
  $HOME/.dotfiles/bin
  $CARGO_HOME/bin
  $GOBIN
  $path
)

export MANPAGER='nvim +Man!'
export ESCDELAY=25
export LESS='-RQKcig -j.5 --incsearch --no-vbell -x4 --use-color -DPw -DEw'
export FZF_DEFAULT_OPTS="--style minimal \
  --info inline-right --color 'bg+:-1,fg+:15,gutter:-1,pointer:4,border:8' \
  --layout=reverse --height 50% --prompt '❯ ' --gutter ' ' \
  --bind 'ctrl-d:preview-half-page-down,ctrl-u:preview-half-page-up,ctrl-e:preview-down'"
