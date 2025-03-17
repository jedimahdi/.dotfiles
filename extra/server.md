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
sudo apt install zoxide gcc make neovim zsh eza ripgrep tmux stow unzip
```

### Nodejs

sudo apt install nodejs npm
mkdir -p ~/.npm-global
npm config set prefix ~/.npm-global
npm i -g prettier

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

## Copy ssh pub to server

ssh-copy-id -i ~/.ssh/id_ed25519.pub mahdi@<host>

## Use v2raya

wget -qO - https://apt.v2raya.org/key/public-key.asc | sudo tee /etc/apt/keyrings/v2raya.asc
echo "deb [signed-by=/etc/apt/keyrings/v2raya.asc] https://apt.v2raya.org/ v2raya main" | sudo tee /etc/apt/sources.list.d/v2raya.list
sudo apt update
sudo apt install v2raya v2ray
v2raya --lite
export http_proxy='http://localhost:20171' && export https_proxy='http://localhost:20171'
or
export http_proxy='socks5://127.0.0.1:20170' && export https_proxy='socks5://127.0.0.1:20170'


## Postgres

```bash
sudo apt install postgresql postgresql-contrib

sudo -i -u postgres
# or sudo -u postgres psql template1

ALTER USER mahdi with encrypted password '1234';
# or \password
```

## Nginx and ssl

sudo apt install nginx
sudo nvim /etc/nginx/sites-available/yourdomain

```nginx
server {
    listen 80;
    server_name yourdomain.com www.yourdomain.com;

    location / {
        proxy_pass http://localhost:3000;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection 'upgrade';
        proxy_set_header Host $host;
        proxy_cache_bypass $http_upgrade;
    }
}
```

To enable:

sudo ln -s /etc/nginx/sites-available/yourdomain /etc/nginx/sites-enabled/

Test and reload:

sudo nginx -t
sudo systemctl reload nginx

Add ssl with certbot:

sudo apt install certbot python3-certbot-nginx
sudo certbot --nginx -d yourdomain.com -d www.yourdomain.com

Firewall:

sudo ufw allow 443
sudo ufw status

Certificate Renewal:

sudo certbot renew --dry-run

## Auto start

### Nodejs

npm install -g pm2
pm2 start npm --name "app-name" -- start
pm2 save
pm2 startup

### Binary

sudo nvim /etc/systemd/system/shop-backend.service

```
[Unit]
Description=Shop Backend
After=network.target

[Service]
Type=simple
User=mahdi
ExecStart=/home/mahdi/shop-backend/bin/api
WorkingDirectory=/home/mahdi/shop-backend
Restart=on-failure
Environment="ENV_VAR=value" # Optional, add environment variables if needed

[Install]
WantedBy=multi-user.target
```

sudo systemctl daemon-reload
sudo systemctl enable --now shop-backend.service
systemctl status shop-backend.service
