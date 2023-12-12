{
  description = "jedi dotfiles";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
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
        overlays = [ ];
      };
      browser = "firefox";
      lib = nixpkgs.lib;
    in
    {
      homeConfigurations = {
        user = home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          modules = [ ./nix/home.nix ]; # load home.nix from selected PROFILE
          extraSpecialArgs = {
            inherit browser;
          };
        };
      };
      nixosConfigurations = {
        system = lib.nixosSystem {
          inherit system;
          modules = [ ./nix/configuration.nix ]; # load configuration.nix from selected PROFILE
          specialArgs = {
            inherit browser;
          };
        };
      };
    };
}
