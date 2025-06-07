
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

## Qt theme

https://wiki.archlinux.org/title/Qt#Configuration_of_Qt_5/6_applications_under_environments_other_than_KDE_Plasma

qt6ct
qt5ct
adwaita-qt5 (aur)
adwaita-qt6 (aur)

## Core

ulimit -c unlimited

sudo su
echo "./core" > /proc/sys/kernel/core_pattern

cat /proc/sys/kernel/core_pattern
