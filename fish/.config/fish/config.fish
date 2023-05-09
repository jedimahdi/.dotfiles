# disable greeting
set fish_greeting

set --export EDITOR nvim
set --export MANPAGER 'nvim +Man!'
set DOTFILES "$HOME/.dotfiles"
# set TERM "tmux-256color"

fish_add_path $DOTFILES/bin
fish_add_path $HOME/.local/bin
# fish_add_path $HOME/.neovim/bin
fish_add_path $HOME/.npm-global/bin
fish_add_path $HOME/.cabal/bin
fish_add_path $HOME/.ghcup/bin
fish_add_path $HOME/go/bin
fish_add_path $HOME/.pack/bin
fish_add_path $HOME/.elan/bin
fish_add_path $HOME/.local/share/coursier/bin

# starship init fish | source
direnv hook fish | source

# abbr -a -g c clear

# pnpm
set -gx PNPM_HOME "/home/mahdi/.local/share/pnpm"
set -gx PATH "$PNPM_HOME" $PATH
# pnpm end
