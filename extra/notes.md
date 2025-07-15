## New User

useradd -m -G wheel -s /bin/zsh user
passwd user

EDITOR=nvim visudo
%wheel ALL=(ALL:ALL) ALL

## Pipewire

packages: pipewire, pipewire-audio, pipewire-alsa, pipewire-pulse, wireplumber
services: pipewire-pulse.service

## Hyprland

packages: hyprland wl-clipboard hyprpaper fuzzel xdg-desktop-portal-hyprland cliphist swappy grim slurp

## SSH

### Generate a key

ssh-keygen -t ed25519 -C "mahdi.se@yahoo.com"

### Start ssh-agent in current shell

eval $(ssh-agent)

ssh-add ~/.ssh/id_ed25519

### Start ssh-agent as service

systemctl --user enable --now ssh-agent.service

### Add host to .ssh/config

Host hostname
HostName host
User mahdi
Port 22
ForwardAgent yes

## GnuPG

gpg --gen-key
gpg --list-keys

gpg --export-secret-keys --armor {fingerprint} > privkey.asc
gpg --export --armor {fingerprint} > pubkey.asc

gpg --import pubkey.asc
gpg --allow-secret-key-import --import privkey.asc

gpg --edit-key {fingerprint}
trust
save

## Qt theme

https://wiki.archlinux.org/title/Qt#Configuration_of_Qt_5/6_applications_under_environments_other_than_KDE_Plasma

qt6ct
qt5ct
adwaita-qt5 (aur)
adwaita-qt6 (aur)

## Core

ulimit -c
ulimit -c unlimited

sudo sysctl -w kernel.core_pattern=core.%p
cat /proc/sys/kernel/core_pattern
sudo nvim /etc/sysctl.d/99-core-pattern.conf
kernel.core_pattern=core.%p

### systemd-coredump

|/usr/lib/systemd/systemd-coredump %P %u %g %s %t %c %h %d %F
coredumpctl list
coredumpctl dump 12345 > core.12345
coredumpctl gdb 12345

remove old journals which also removes systemc
sudo journalctl --vacuum-size=100M
sudo journalctl --vacuum-time=7d

## Colors

https://github.com/sharkdp/pastel
sudo pacman -S pastel

## Xdg Autostart

watch out for /etc/xdg/autostart
