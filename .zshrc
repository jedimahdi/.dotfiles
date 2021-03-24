# Enable colors and change prompt:
autoload -U colors && colors	# Load colors
fpath+=$HOME/.zsh/pure

autoload -U promptinit; promptinit
prompt pure

setopt autocd		# Automatically cd into typed directory.
stty stop undef		# Disable ctrl-s to freeze terminal.
setopt interactive_comments

# Lines configured by zsh-newuser-install
HISTFILE=~/.cache/zsh/history
HISTSIZE=10000
SAVEHIST=10000

# Basic auto/tab complete:
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)		# Include hidden files.

bindkey -e


# aliases
# Verbosity and settings that you pretty much just always are going to want.

# sudo not required for some system commands
for x in mount umount sv pacman updatedb su ; do
	alias $x="sudo $x"
done

alias \
	cp="cp -iv" \
	mv="mv -iv" \
	rm="rm -vI" \
	bc="bc -ql" \
	mkd="mkdir -pv" \
	yt="youtube-dl --add-metadata -i" \
	yta="yt -x -f bestaudio/best" \
	ffmpeg="ffmpeg -hide_banner"

# Colorize commands when possible.
alias \
	ls="ls -hN --color=auto --group-directories-first" \
	grep="grep --color=auto" \
	diff="diff --color=auto" \
	ccat="highlight --out-format=ansi"

alias e="$EDITOR"
alias v="$EDITOR"
alias p="sudo pacman"
alias c="clear"
alias gs="git status"

# Load syntax highlighting; should be last.
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh 2>/dev/null
source ~/.zsh/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh 2>/dev/null
