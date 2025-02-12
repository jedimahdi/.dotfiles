# Ubuntu server

## Update

```bash
apt update
apt upgrade
apt dist-upgrade
do-release-upgrade
iptables -I INPUT -p tcp --dport 1022 -j ACCEPT
```

## Add user

```bash
useradd -m mahdi
passwd mahdi
usermod -aG sudo mahdi
```

## Bring in dotfiles

```bash
mkdir -p ~/.config
mkdir -p ~/.local/share/zsh/plugins

git clone https://github.com/jedimahdi/nvim.git ~/.config/nvim
git clone https://github.com/jedimahdi/.dotfiles.git ~/.dotfiles
stow -d ~/.dotfiles
```

## Packages

```bash
sudo apt install zoxide gcc make neovim zsh eza ripgrep tmux stow
```

## Fzf

```bash
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
~/.fzf/install
echo 'export PATH="$HOME/.fzf/bin:$PATH"' >> .zshrc
```

## Starship

```bash
curl -sS https://starship.rs/install.sh | sh
```
