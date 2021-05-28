# utils
pacman -S base-devel git make wget curl gcc xz coreutils xcape nitrogen ripgrep fd sysstat dunst zip unzip tar unrar openvpn python-pip volumeicon pulseaudio pulseaudio-alsa alsa-utils playerctl pcmanfm networkmanager networkmanager-openvpn networkmanager-openconnect networkmanager-pptp network-manager-applet --needed --noconfirm
pacman -S fzf python-i3ipc picom xdotool tree-sitter npm

# window manager
pacman -S i3 dmenu rofi --needed --noconfirm

# login
pacman -S lightdm lightdm-gtk-greeter --needed --noconfirm

# fonts
pacman -S ttf-windows ttf-droid --needed --noconfirm

# terminal
pacman -S alacritty kitty fish tmux neovim python-pynvim --needed --noconfirm

# gui
pacman -S firefox xarchiver mpc mpv lxappearance --needed --noconfirm
pacman -S telegram-desktop xfce4-screenshooter uget
pacman -S zathura zathura-pdf-mupdf zathura-ps

## Services
systemctl enable lightdm


## Aur (paru)
git clone https://aur.archlinux.org/paru.git
cd paru
makepkg -si

# Aur Packages: sweet-theme-dark volantes-cursors flatery-icon-theme-git
# font-manager ttf-iosevka nerd-fonts-source-code-pro nerd-fonts-iosevka
# spotify google-chrome
# libjwt perl-app-sqitch

# Paq 
git clone https://github.com/savq/paq-nvim.git \
    "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/pack/paqs/opt/paq-nvim

# ghcup
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# lua language server
git clone https://github.com/sumneko/lua-language-server
cd lua-language-server
git submodule update --init --recursive
cd 3rd/luamake
ninja -f ninja/macos.ninja
cd ../..
./3rd/luamake/luamake rebuild


# npm (add to PATH: ~/.npm-global/bin)
mkdir ~/.npm-global
npm config set prefix '~/.npm-global'


# typescript language server
npm install -g typescript typescript-language-server

npm install -g neovim

# cabal exe
mkdir cabal-system-exe
cd cabal-system-exe
cabal init
curl https://www.stackage.org/lts-16.31/cabal.config > cabal.project.freeze
cabal install brittany
cabal install hpack-dhall
cabal install ghcid

# headroom problem arch linux
sudo ln -s /usr/lib/libpcre.so /usr/lib/libpcre.so.3

# Snap
paru -S snapd
sudo systemctl enable --now snapd.socket

# postgres db
pacman -S postgresql perl-dbd-pg
sudo -iu postgres
initdb --locale=en_US.UTF-8 -E UTF8 -D /var/lib/postgres/data
sudo systemctl start postgresql.service
# [postgres]$ createuser --interactive
sudo snap install postbird


# ZSH
pacman -S zsh
mkdir ~/.zsh
git clone https://github.com/sindresorhus/pure.git ~/.zsh/pure
git clone https://github.com/zdharma/fast-syntax-highlighting ~/.zsh/fast-syntax-highlighting
git clone https://github.com/zsh-users/zsh-autosuggestions ~/.zsh/zsh-autosuggestions
mkdir ~/.cache/zsh

# git
git config --global credential.helper store

# purescript
yay -S ncurses5-compat-libs
npm install -g purescript
npm install -g spago
npm install -g purescript-language-server
