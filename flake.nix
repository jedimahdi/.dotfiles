{
  description = "jedi dotfiles";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-colors.url = "github:misterio77/nix-colors";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    hyprland = {
      url = "github:hyprwm/Hyprland";
    };
    nix-index-database = {
      url = "github:Mic92/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    manix = {
      url = "github:nix-community/manix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, rust-overlay, treefmt-nix, ... }@inputs:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config = {
          allowUnfree = true;
          allowUnfreePredicate = _: true;
        };
        overlays = [
          (final: _: { project.haskellPackages = final.haskell.packages.ghc948; })
          (import rust-overlay)
          (_: final: { vaapiIntel = final.vaapiIntel.override { enableHybridCodec = true; }; })
        ];
      };
      treefmtEval = treefmt-nix.lib.evalModule pkgs {
        projectRootFile = "flake.nix";
        programs = {
          nixpkgs-fmt.enable = true;
          shfmt.enable = true;
        };
      };
      browser = "firefox";
      inherit (nixpkgs) lib;
      inherit (pkgs.project) haskellPackages;
    in
    {
      homeConfigurations = {
        user = home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          modules = [ ./home-manager/home.nix ];
          extraSpecialArgs = {
            inherit inputs;
            inherit browser;
          };
        };
      };
      nixosConfigurations = {
        system = lib.nixosSystem {
          inherit system;
          modules = [ ./nixos/configuration.nix ];
          specialArgs = {
            inherit inputs;
            inherit browser;
            inherit haskellPackages;
          };
        };
      };
      formatter.${system} = treefmtEval.config.build.wrapper;
      devShells.${system}.default = pkgs.mkShell {
        name = "dotfiles";
        nativeBuildInputs = builtins.attrValues {
          treefmt = treefmtEval.config.build.wrapper;
          inherit (pkgs) shellcheck shfmt nixpkgs-fmt;
        };
      };
    };
}
