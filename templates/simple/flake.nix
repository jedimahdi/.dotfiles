{
  description = "Simple environment";

  inputs = {
    nixpkgs-stable.url = "github:NixOS/nixpkgs/release-22.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    nixpkgs-stable,
    nixpkgs-unstable,
    utils,
    ...
  }: let
    name = throw "Put your package name here.";
  in
    utils.lib.eachDefaultSystem (system: let
      pkgs = {
        stable = import nixpkgs-stable {inherit system;};
        unstable = import nixpkgs-unstable {inherit system;};
      };
    in {
      devShell = import ./shell.nix {
        inherit name pkgs;
      };
    });
}
