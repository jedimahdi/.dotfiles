export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_STATE_HOME="$HOME/.local/state"

export ZDOTDIR="$XDG_CONFIG_HOME/zsh"

# history files
export LESSHISTFILE="$XDG_CACHE_HOME/less_history"
export PYTHON_HISTORY="$XDG_DATA_HOME/python/history"

export PATH="$HOME/.dotfiles/bin:$PATH"
# export PATH="$HOME/.nvim/bin:$PATH"
export PATH="$CARGO_HOME/bin:$PATH"
export PATH="$GOBIN:$PATH"
export PATH="$HOME/.npm-global/bin:$PATH"

export FZF_DEFAULT_OPTS="--style minimal --info inline-right --color 'bg+:-1,fg+:15,gutter:-1,pointer:4,border:8' --border rounded --layout=reverse --height 90% --tmux 100% --prompt '❯ '"
export FZF_CTRL_R_OPTS="--no-preview"
export MANPAGER='nvim +Man!'
export MANWIDTH=999

export ESCDELAY=25
export LESS='-cigRS -j4 -x4 -#5 -z-10'
export LESSPROMPT='?f%f .?ltLine %lt:?pt%pt\%:?btByte %bt:-...'
