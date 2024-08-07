{ pkgs, inputs, wm, ... }: {
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
    ./wm/${wm}.nix
  ];

  nixpkgs.config.allowUnfree = true;

  documentation = {
    enable = true;
    man.enable = true;
    dev.enable = true;
  };

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
      man-pages
      qt5.qtwayland
    ];

    shells = with pkgs; [ zsh ];
  };

  hardware = {
    graphics = {
      enable = true;
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
    extraGroups = [ "adbusers" "input" "libvirtd" "networkmanager" "plugdev" "transmission" "video" "wheel" "kvm" ];
  };
  users.defaultUserShell = pkgs.zsh;

  hardware.bluetooth.enable = true;
  services = {
    thermald.enable = true;
    blueman.enable = true;
    # openssh.enable = true;
  };
  programs = {
    dconf = {
      enable = true;
    };
    zsh.enable = true;
    # gnupg.agent = {
    #   enable = true;
    #   enableSSHSupport = true;
    # };
  };

  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05";

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;
}
