#!/bin/bash

sudo pacman -S --needed --noconfirm git base-devel
mkdir -p ~/tmp
cd ~/tmp
git clone https://aur.archlinux.org/yay.git yay
cd yay
makepkg -si
cd ..
rm -rf yay
