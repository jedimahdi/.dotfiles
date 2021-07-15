#!/bin/bash

sudo pacman -S npm --needed --noconfirm
mkdir -p ~/.npm-global
npm config set prefix '~/.npm-global'
