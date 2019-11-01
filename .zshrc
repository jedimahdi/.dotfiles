export ZSH=$HOME/.oh-my-zsh

TERM=xterm-256color
export TERM=xterm-256color

export GOPATH=$HOME/apps/go
export GOBIN=$GOPATH/bin
PATH=$PATH:$GOPATH:$GOBIN

export NVIM_GTK_NO_HEADERBAR=1
export NVIM_GTK_PREFER_DARK_THEME=1
export NVIM_GTK_NO_WINDOW_DECORATION=1

fpath=( "$HOME/.zfunctions" $fpath )

# Fzf respect .gitignore
export FZF_DEFAULT_COMMAND='fd --type f --hidden --follow --exclude .git --exclude node_modules --exclude /.idea'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

# Setting rg as the default source for fzf
# export FZF_DEFAULT_COMMAND='rg --files'

# Apply the command to CTRL-T as well
# export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"


ZSH_THEME="spaceship"
#ZSH_THEME="robbyrussell"
# export SPACESHIP_CHAR_SYMBOL="❯ "
# export SPACESHIP_JOBS_SYMBOL="»"
# export SPACESHIP_TIME_SHOW=false
# export SPACESHIP_USER_PREFIX="as "
# export SPACESHIP_USER_SHOW="needed"
# export SPACESHIP_DIR_TRUNC_PREFIX=".../"
# export SPACESHIP_DIR_TRUNC_REPO=false

source ~/zsh/git_simple.zsh
source ~/zsh/virtualenv.zsh


SPACESHIP_PROMPT_ORDER=(
  virtualenv
  user          # Username section
  dir           # Current directory section
  host          # Hostname section
  git_simple    # Custom Git section
  line_sep      # Line break
  jobs          # Background jobs indicator
  exit_code     # Exit code section
  char          # Prompt character
)
# Hide prefixes before prompt sections
SPACESHIP_PROMPT_PREFIXES_SHOW=true
SPACESHIP_CHAR_SYMBOL="❯ "
# SPACESHIP_CHAR_SYMBOL="$ "
SPACESHIP_CHAR_SUFFIX=""
SPACESHIP_DIR_COLOR="white"


PATH="$PATH:$(ruby -e 'print Gem.user_dir')/bin"
PATH="$PATH:$HOME/.config/composer/vendor/bin"

export ANDROID_HOME=$HOME/Android/Sdk
export PATH=$PATH:$ANDROID_HOME/emulator
export PATH=$PATH:$ANDROID_HOME/tools
export PATH=$PATH:$ANDROID_HOME/tools/bin
export PATH=$PATH:$ANDROID_HOME/platform-tools

# Command auto-correction.
ENABLE_CORRECTION="false"

# Command execution time stamp shown in the history command output.
HIST_STAMPS="mm/dd/yyyy"

plugins=(
  git
  zsh-syntax-highlighting
  zsh-completions
  zsh-autosuggestions
  history-substring-search
)

source "$HOME/.zsh/plugins/zsh-system-clipboard.zsh"
source $ZSH/oh-my-zsh.sh
source $HOME/.zsh/zsh_aliases

# Enable tab completion for flags by entering following line to your shell configuration file (~/.bashrc or ~/.zshrc) :
source $(dirname $(gem which colorls))/tab_complete.sh

# # Syntax highlighting and tab completion
autoload -U compinit && compinit

# autoload -U promptinit; promptinit
# prompt pure

export LANG=en_US.UTF-8

export EDITOR='nvim'
export TERMINAL="st"
PROMPT_COMMAND="$PROMPT_COMMAND; pwd > /tmp/whereami"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
