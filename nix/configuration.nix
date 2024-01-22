{ pkgs, ... }:

{
  imports =
    [
      ./system/hardware.nix
      ./system/locale.nix
      ./system/nvidia-disable.nix
      ./system/nix.nix
      ./system/fonts.nix
      ./system/sound.nix
      ./system/wm/hyprland.nix
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
    sessionVariables = {
      VDPAU_DRIVER = "va_gl";
    };

    systemPackages = with pkgs; [
      vim
      wget
      neovim
      zsh
      git
      stow
      home-manager
      dunst
      libnotify
      pciutils
      firefox
    ];

    shells = with pkgs; [ zsh ];
  };

  hardware = {
    opengl = {
      enable = true;
      driSupport = true;
      extraPackages = with pkgs; [
        libva
        libvdpau-va-gl
        intel-media-driver
        vaapiVdpau
        vaapiIntel
        amdvlk
      ];
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
      videoDrivers = [ "amdgpu" ];
      autoRepeatDelay = 300;
      autoRepeatInterval = 50;
      layout = "us";
      xkbVariant = "";
    };
    hoogle = {
      enable = true;
      packages = hp: with hp; [ lens ];
      # haskellPackages = haskellPackages;
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
