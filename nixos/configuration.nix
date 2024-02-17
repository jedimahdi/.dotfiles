{ pkgs, inputs, ... }: {
  imports = [
    ./hardware.nix
    inputs.nixos-hardware.nixosModules.common-pc-laptop
    inputs.nixos-hardware.nixosModules.common-pc-laptop-ssd
    inputs.nixos-hardware.nixosModules.common-cpu-intel
    inputs.nixos-hardware.nixosModules.common-gpu-amd
    ./locale.nix
    ./nix.nix
    ./fonts.nix
    ./sound.nix
    ./wm/hyprland.nix
  ];

  nixpkgs.config.allowUnfree = true;

  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
  };
  networking = {
    hostName = "nixos";
    networkmanager.enable = true;
  };
  environment = {
    # sessionVariables = {
    #   VDPAU_DRIVER = "va_gl";
    # };

    systemPackages = with pkgs; [
      vim
      wget
      neovim
      zsh
      git
      home-manager
      libnotify
      pciutils
    ];

    shells = with pkgs; [ zsh ];
  };

  hardware = {
    opengl = {
      enable = true;
      driSupport = true;
      # extraPackages = with pkgs; [
      #   libva
      #   libvdpau-va-gl
      #   intel-media-driver
      #   vaapiVdpau
      #   vaapiIntel
      #   amdvlk
      # ];
    };
  };

  users.users.mahdi = {
    isNormalUser = true;
    description = "mahdi";
    extraGroups = [ "networkmanager" "wheel" ];
  };
  users.defaultUserShell = pkgs.zsh;

  hardware.bluetooth.enable = true;
  services = {
    thermald.enable = true;
    xserver = {
      enable = true;
      # videoDrivers = [ "amdgpu" ];
      autoRepeatDelay = 300;
      autoRepeatInterval = 50;
      xkb = {
        layout = "us";
        variant = "";
      };
    };
    dbus = {
      enable = true;
      packages = [ pkgs.dconf ];
    };
    blueman.enable = true;
    openssh.enable = true;
  };
  programs = {
    dconf = {
      enable = true;
    };
    zsh.enable = true;
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };
  };

  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11";

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;
}
