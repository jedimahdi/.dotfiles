{
  description = "jedi dotfiles";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    stylix = {
      url = "github:danth/stylix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };
    hyprland = {
      url = "github:hyprwm/Hyprland";
    };
    nix-index-database = {
      url = "github:Mic92/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    rycee-nurpkgs = {
      url = "gitlab:rycee/nur-expressions?dir=pkgs/firefox-addons";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, treefmt-nix, ... }@inputs:
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
        system = nixpkgs.lib.nixosSystem {
          inherit system;
          modules = [ ./nixos/configuration.nix ];
          specialArgs = {
            inherit inputs;
            inherit browser;
          };
        };
      };
      formatter.${system} = treefmtEval.config.build.wrapper;
      devShells.${system}.default = pkgs.mkShell {
        name = "dotfiles";
        nativeBuildInputs = [
          treefmtEval.config.build.wrapper
          pkgs.nixpkgs-fmt
          pkgs.shfmt
          pkgs.shellcheck
        ];
      };
    };
}
