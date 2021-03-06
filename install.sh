## Packages

# utils
pacman -S base-devel git make wget curl gcc xz coreutils xcape nitrogen ripgrep fd sysstat dunst zip unzip tar unrar openvpn python-pip volumeicon pulseaudio pulseaudio-alsa alsa-utils playerctl pcmanfm networkmanager networkmanager-openvpn networkmanager-openconnect networkmanager-pptp network-manager-applet --needed --noconfirm

# window manager
pacman -S i3 dmenu rofi --needed --noconfirm

# login
pacman -S lightdm lightdm-gtk-greeter --needed --noconfirm

# fonts
pacman -S ttf-iosevka ttf-windows --needed --noconfirm

# terminal
pacman -S alacritty kitty fish tmux neovim python-pynvim --needed --noconfirm

# gui
pacman -S firefox xarchiver mpv lxappearance --needed --noconfirm

## Services
systemctl enable lightdm


## Aur (paru)
git clone https://aur.archlinux.org/paru.git
cd paru
makepkg -si

# sweet-theme-dark volantes-cursors flatery-icon-theme-git 
