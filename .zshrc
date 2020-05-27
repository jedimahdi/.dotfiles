export ZSH=$HOME/.oh-my-zsh

if [[ -d $HOME/.zsh/functions ]]; then
    for func in $HOME/.zsh/functions/*(:t); autoload -U $func
fi

########################################################
# Configuration
########################################################

COLOR_BLACK="\e[0;30m"
COLOR_BLUE="\e[0;34m"
COLOR_GREEN="\e[0;32m"
COLOR_CYAN="\e[0;36m"
COLOR_PINK="\e[0;35m"
COLOR_RED="\e[0;31m"
COLOR_PURPLE="\e[0;35m"
COLOR_BROWN="\e[0;33m"
COLOR_LIGHTGRAY="\e[0;37m"
COLOR_DARKGRAY="\e[1;30m"
COLOR_LIGHTBLUE="\e[1;34m"
COLOR_LIGHTGREEN="\e[1;32m"
COLOR_LIGHTCYAN="\e[1;36m"
COLOR_LIGHTRED="\e[1;31m"
COLOR_LIGHTPURPLE="\e[1;35m"
COLOR_YELLOW="\e[1;33m"
COLOR_WHITE="\e[1;37m"
COLOR_NONE="\e[0m"

if [ -z "$TMUX" ]; then
    export TERM=xterm-256color-italic
else
    export TERM=tmux-256color
fi
# export TERM=xterm-256color

# initialize autocomplete
autoload -U compinit add-zsh-hook
compinit

PATH="$PATH:$(ruby -e 'print Gem.user_dir')/bin"
PATH="$PATH:$HOME/.config/composer/vendor/bin"
export ANDROID_HOME=$HOME/Android/Sdk
export PATH=$PATH:$ANDROID_HOME/emulator
export PATH=$PATH:$ANDROID_HOME/tools
export PATH=$PATH:$ANDROID_HOME/tools/bin
export PATH=$PATH:$ANDROID_HOME/platform-tools
export PATH=$PATH:$HOME/.local/bin/statusbar
export PATH=$PATH:~/.npm-global/bin
export DENO_INSTALL="/home/mahdi/.deno"
export PATH="$DENO_INSTALL/bin:$PATH"
export GOPATH=$HOME/apps/go
export GOBIN=$GOPATH/bin
PATH=$PATH:$GOPATH:$GOBIN

# display how long all tasks over 10 seconds take
export REPORTTIME=10

setopt NO_BG_NICE
setopt NO_HUP
setopt NO_LIST_BEEP
setopt LOCAL_OPTIONS
setopt LOCAL_TRAPS
setopt PROMPT_SUBST

# history
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt EXTENDED_HISTORY
setopt HIST_REDUCE_BLANKS
setopt SHARE_HISTORY
setopt HIST_IGNORE_ALL_DUPS

setopt COMPLETE_ALIASES

# make terminal command navigation sane again
bindkey '^[^[[D' backward-word
bindkey '^[^[[C' forward-word
bindkey '^[[5D' beginning-of-line
bindkey '^[[5C' end-of-line
bindkey '^[[3~' delete-char
bindkey '^?' backward-delete-char
bindkey "^A" vi-beginning-of-line

# matches case insensitive for lowercase
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# pasting with tabs doesn't perform completion
zstyle ':completion:*' insert-tab pending

# default to file completion
zstyle ':completion:*' completer _expand _complete _files _correct _approximate

# source all .zsh files inside of the zsh/ directory
source "$HOME/.zsh/utils.zsh"
source "$HOME/.zsh/prompt.zsh"
source "$HOME/.zsh/zsh_aliases"


fpath=( "$HOME/.zsh/functions" $fpath )

########################################################
# Plugin setup
########################################################

export CACHEDIR="$HOME/.local/share"
export ZPLUGDIR="$CACHEDIR/zsh/plugins"
[[ -d "$ZPLUGDIR" ]] || mkdir -p "$ZPLUGDIR"
# array containing plugin information (managed by zfetch)
typeset -A plugins

zfetch $ZPLUGDIR zsh-users/zsh-syntax-highlighting
zfetch $ZPLUGDIR zsh-users/zsh-autosuggestions
zfetch $ZPLUGDIR chriskempson/base16-shell

# if [ "$FNM_LOADED" != "true" ]; then
#     eval "`fnm env --multi --use-on-cd`"
#     export FNM_LOADED="true"
# fi

[[ -e ~/.terminfo ]] && export TERMINFO_DIRS=~/.terminfo:/usr/share/terminfo
########################################################
# Setup
########################################################
#

# add a config file for ripgrep
export RIPGREP_CONFIG_PATH="$HOME/.rgrc"


[ -f $HOME/.fzf.zsh ] && source $HOME/.fzf.zsh
export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow -g "!{.git,node_modules}/*" 2> /dev/null'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

# move to next word with ctrl-F
bindkey -M viins "^F" vi-forward-word
# Move to end of line with ctrl-E
bindkey -M viins "^E" vi-add-eol


# add color to man pages
export MANROFFOPT='-c'
export LESS_TERMCAP_mb=$(tput bold; tput setaf 2)
export LESS_TERMCAP_md=$(tput bold; tput setaf 6)
export LESS_TERMCAP_me=$(tput sgr0)
export LESS_TERMCAP_so=$(tput bold; tput setaf 3; tput setab 4)
export LESS_TERMCAP_se=$(tput rmso; tput sgr0)
export LESS_TERMCAP_us=$(tput smul; tput bold; tput setaf 7)
export LESS_TERMCAP_ue=$(tput rmul; tput sgr0)
export LESS_TERMCAP_mr=$(tput rev)
export LESS_TERMCAP_mh=$(tput dim)


# Command auto-correction.
ENABLE_CORRECTION="false"

# Command execution time stamp shown in the history command output.
# HIST_STAMPS="mm/dd/yyyy"

# plugins=(
#   git
#   zsh-syntax-highlighting
#   zsh-completions
#   zsh-autosuggestions
#   history-substring-search
# )

#source "$HOME/.zsh/plugins/zsh-system-clipboard.zsh"
# source $ZSH/oh-my-zsh.sh

# Enable tab completion for flags by entering following line to your shell configuration file (~/.bashrc or ~/.zshrc) :
#source $(dirname $(gem which colorls))/tab_complete.sh

# # Syntax highlighting and tab completion
# autoload -U compinit && compinit

# autoload -U promptinit; promptinit
# prompt pure

export LANG=en_US.UTF-8

export EDITOR='nvim'
export TERMINAL="st"
export BROWSER="chromium"
PROMPT_COMMAND="$PROMPT_COMMAND; pwd > /tmp/whereami"

if [ -e /home/mahdi/.nix-profile/etc/profile.d/nix.sh ]; then . /home/mahdi/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
