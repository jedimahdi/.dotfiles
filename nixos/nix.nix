{ pkgs, lib, inputs, ... }: {
  environment.variables.NIXPKGS_ALLOW_UNFREE = "1";
  nix =
    let
      flakes = lib.filterAttrs (name: value: value ? outputs) inputs;

      nixRegistry = builtins.mapAttrs
        (_: v: { flake = v; })
        flakes;
    in
    {
      package = pkgs.nix;
      registry = nixRegistry;
      nixPath = [ "/etc/nix/inputs" ];

      settings = {
        experimental-features = "nix-command flakes";
        substituters = [
          "https://nix-community.cachix.org"
          "https://cache.nixos.org/"
        ];
        trusted-public-keys = [
          "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        ];
        keep-outputs = true;
        keep-derivations = true;
        warn-dirty = false;
        # Deduplicate and optimize nix store
        auto-optimise-store = true;
        trusted-users = [ "root" "@wheel" ];
        allowed-users = [ "root" "@wheel" ];
      };
    };

  # This will additionally add your inputs to the system's legacy channels
  # Making legacy nix commands consistent as well, awesome!
  environment.etc = lib.mapAttrs'
    (name: value: { name = "nix/inputs/${name}"; value = { source = value.outPath; }; })
    inputs;

  programs.nix-ld = {
    enable = true;
    libraries = with pkgs; [
      # alsa-lib
      # at-spi2-atk
      # at-spi2-core
      # atk
      cairo
      # cups
      curl
      # dbus
      # expat
      # fontconfig
      freetype
      # fuse3
      # gdk-pixbuf
      # glib
      # gtk3
      # icu
      # libGL
      # libappindicator-gtk3
      # libdrm
      # libglvnd
      libnotify
      # libpulseaudio
      # libunwind
      # libusb1
      # libuuid
      # libxkbcommon
      # mesa
      # nspr
      # nss
      openssl
      pango
      # pipewire
      stdenv.cc.cc
      # systemd
      vulkan-loader
      xorg.libX11
      # xorg.libXScrnSaver
      # xorg.libXcomposite
      # xorg.libXcursor
      # xorg.libXdamage
      # xorg.libXext
      # xorg.libXfixes
      # xorg.libXi
      # xorg.libXrandr
      # xorg.libXrender
      # xorg.libXtst
      # xorg.libxcb
      # xorg.libxkbfile
      # xorg.libxshmfence
      zlib
    ];
  };
}
