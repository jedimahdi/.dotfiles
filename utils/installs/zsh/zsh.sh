#!/bin/bash

sudo pacman -S zsh --needed --noconfirm
mkdir -p ~/.cache/zsh
chsh -s $(which zsh)
