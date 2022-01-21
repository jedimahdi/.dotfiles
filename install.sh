#!/usr/bin/env bash

DOTFILES="$HOME/.dotfiles"
COLOR_GRAY="\033[1;38;5;243m"
COLOR_BLUE="\033[1;34m"
COLOR_GREEN="\033[1;32m"
COLOR_RED="\033[1;31m"
COLOR_PURPLE="\033[1;35m"
COLOR_YELLOW="\033[1;33m"
COLOR_NONE="\033[0m"

title() {
	echo -e "\n${COLOR_PURPLE}$1${COLOR_NONE}"
	echo -e "${COLOR_GRAY}==============================${COLOR_NONE}\n"
}

info() {
	echo -e "${COLOR_BLUE}Info: ${COLOR_NONE}$1"
}

success() {
	echo -e "${COLOR_GREEN}$1${COLOR_NONE}"
}

get_linkables() {
	find -H "$DOTFILES" -maxdepth 1 -mindepth 1 -type d -not -path '*/bin' -not -path '*/utils' -not -path '*/.git' -not -path '*/resources' | cut -d/ -f5
}

make_symlink() {
	stow -d "$DOTFILES" -t "$HOME" "$1"
}

iscmd() {
	command -v "$@" >&-
}

setup_symlinks() {
	title "Creating symlinks"

	if [ ! -d "$HOME/.config" ]; then
		info "Creating ~/.config"
		mkdir -p "$HOME/.config"
	fi

	for folder in $(get_linkables); do
		info "Creating symlink for $folder"
		make_symlink "$folder"
	done
}

setup_git() {
	title "Setting up Git"

	defaultName=$(git config user.name)
	defaultEmail=$(git config user.email)
	defaultGithub=$(git config github.user)

	read -rp "Name [$defaultName] " name
	read -rp "Email [$defaultEmail] " email
	read -rp "Github username [$defaultGithub] " github

	git config -f ~/.gitconfig-local user.name "${name:-$defaultName}"
	git config -f ~/.gitconfig-local user.email "${email:-$defaultEmail}"
	git config -f ~/.gitconfig-local github.user "${github:-$defaultGithub}"

	read -rn 1 -p "Save user and password to an unencrypted file to avoid writing? [y/N] " save
	if [[ $save =~ ^([Yy])$ ]]; then
		git config --global credential.helper "store"
	else
		git config --global credential.helper "cache --timeout 3600"
	fi
}

setup_arch() {
	title "Configuring Arch Linux"
	if [[ "$(uname)" == "Linux" ]]; then
		info "Installing general"
		sudo pacman -S --noconfirm --needed base-devel git make wget curl xdotool gcc xz coreutils xcape nitrogen ripgrep fd sysstat dunst zip unzip tar unrar openvpn python-pip volumeicon pulseaudio pulseaudio-alsa alsa-utils playerctl pcmanfm networkmanager networkmanager-openvpn networkmanager-openconnect networkmanager-pptp network-manager-applet ninja cmake
		sudo pacman -S --noconfirm --needed tmux z gnupg bat xclip zsh rsync npm nodejs kitty gnome-keyring python-pynvim xfce4-screenshooter
		sudo pacman -S --noconfirm --needed firefox xarchiver mpv lxappearance

		if [ ! -d "$HOME/tmp" ]; then
			info "Creating ~/tmp"
			mkdir -p "$HOME/tmp"
		fi

		# Installing pikaur
		if test ! "$(command -v pikaur)"; then
			echo -e
			info "Pikaur not installed. Installing."

			cd "$HOME/tmp" || return

			if [ -d "$HOME/tmp/pikaur" ]; then
				info "Remove pikaur from tmp"
				rm -rf "$HOME/tmp/pikaur"
			fi

			git clone https://aur.archlinux.org/pikaur.git
			cd pikaur || return
			makepkg -fsri --noconfirm --needed
			cd "$DOTFILES" || return
		fi

		# Install fzf
		if test ! "$(command -v fzf)"; then
			echo -e
			info "fzf not installed. Installing."

			if [ -d "$HOME/.fzf" ]; then
				info "Remove fzf from ~/.fzf"
				rm -rf "$HOME/.fzf"
			fi

			git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
			~/.fzf/install --key-bindings --completion --no-update-rc --no-bash --no-fish --no-zsh
		fi

		echo -e
		info "Installing neovim dependensies"
		sudo pacman -S --noconfirm --needed vim shfmt
		sudo pikaur -S --noconfirm --needed shellcheck-bin stylua-bin

		echo -e
		info "Installing git dependensies"
		sudo pacman -S --noconfirm --needed git git-delta

		echo -e
		info "Installing zsh dependensies"
		sudo pacman -S --noconfirm --needed zsh

		echo -e
		info "Installing fonts"
		sudo pikaur -S --noconfirm --needed font-victor-mono vazir-fonts ttf-jetbrains-mono

		echo -e
		info "Installing other"
		sudo pikaur -S --noconfirm --needed ncurses5-compat-libs
	else
		warning "Linux not detected. Skipping."
	fi
}

installs=('git' 'link' 'arch' 'all')
install=$(echo "${installs[@]}" | tr ' ' '\n' | fzf)

case "$install" in
link)
	setup_symlinks
	;;
git)
	setup_git
	;;
arch)
	setup_arch
	;;
all)
	setup_arch
	setup_symlinks
	setup_git
	;;
*)
	echo -e $"\nUsage: $(basename "$0") {backup|link|git|homebrew|shell|terminfo|macos|all}\n"
	exit 1
	;;
esac

echo -e
success "Done."
