{
  description = "User configuration flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nur.url = "github:nix-community/NUR";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, nur, ... }: {
    homeConfigurations = {
      mahdi = inputs.home-manager.lib.homeManagerConfiguration {
        pkgs = inputs.nixpkgs.legacyPackages."x86_64-linux";
        modules = [
          ./home.nix
          { nixpkgs.overlays = [ nur.overlay ]; }
        ];
      };
    };
    mahdi = self.homeConfigurations.mahdi.activationPackage;
    defaultPackage.x86_64-linux = self.mahdi;
  };
}
