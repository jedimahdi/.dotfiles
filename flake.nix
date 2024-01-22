{
  description = "jedi dotfiles";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
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
  };

  outputs = { self, nixpkgs, home-manager, rust-overlay, pre-commit-hooks, ... }@inputs:
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
      browser = "firefox";
      inherit (nixpkgs) lib;
      inherit (pkgs.project) haskellPackages;
    in
    {
      homeConfigurations = {
        user = home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          modules = [ ./nix/home.nix ];
          extraSpecialArgs = {
            inherit inputs;
            inherit browser;
          };
        };
      };
      nixosConfigurations = {
        system = lib.nixosSystem {
          inherit system;
          modules = [ ./nix/configuration.nix ];
          specialArgs = {
            inherit inputs;
            inherit browser;
            inherit haskellPackages;
          };
        };
      };
      checks.${system} = {
        pre-commit-check = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            nixpkgs-fmt.enable = true;
            stylua.enable = true;
            shellcheck.enable = true;
            shfmt.enable = true;
          };
        };
      };
      devShells.${system}.default = pkgs.mkShell
        {
          name = "dotfiles";
          buildInputs = with pkgs; [ zlib.dev ];
          inherit (self.checks.${system}.pre-commit-check) shellHook;
          nativeBuildInputs = builtins.attrValues {
            inherit (pre-commit-hooks.packages.${system})
              nixpkgs-fmt stylua shellcheck shfmt;
          };
        };
    };
}
