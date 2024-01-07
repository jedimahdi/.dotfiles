sudo nix-env --list-generations --profile /nix/var/nix/profiles/system
sudo nix-env --profile /nix/var/nix/profiles/system --delete-generations 1 2 3
sudo nix-env --profile /nix/var/nix/profiles/system --delete-generations {1..3}
home-manager switch --flake .#user
sudo nixos-rebuild switch --flake .#system

