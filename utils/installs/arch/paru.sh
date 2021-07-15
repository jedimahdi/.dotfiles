#!/bin/bash

mkdir -p ~/tmp
cd ~/tmp
git clone https://aur.archlinux.org/paru.git paru
cd paru
makepkg -si
cd ../
rm -rf paru

