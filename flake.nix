{
  description = "jedi dotfiles";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-colors.url = "github:misterio77/nix-colors";
  };

  outputs = { self, nixpkgs, home-manager, ... }@inputs:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config = {
          allowUnfree = true;
          allowUnfreePredicate = (_: true);
        };
        overlays = [
          (final: _: { project.haskellPackages = final.haskell.packages.ghc948; })
        ];
      };
      browser = "firefox";
      lib = nixpkgs.lib;
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
          };
        };
      };

      devShells.x86_64-linux.default = pkgs.mkShell
        {
          name = "dotfiles";
          buildInputs = with pkgs; [ zlib.dev ];
        };
    };
}
