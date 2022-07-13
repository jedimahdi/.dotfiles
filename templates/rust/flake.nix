{
  inputs = {
    naersk.url = "github:nix-community/naersk/master";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, utils, naersk, ... }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        naersk-lib = pkgs.callPackage naersk { };
      in
      rec {
        # `nix build`
        packages.hello_world = naersk-lib.buildPackage {
          pname = "hello_world";
          root = ./.;
        };
        defaultPackage = packages.hello_world;

        # `nix run`
        apps.hello_world = utils.lib.mkApp {
          drv = packages.hello_world;
        };
        defaultApp = apps.hello_world;

        # `nix develop`
        devShell = with pkgs; mkShell {
          buildInputs = [ cargo rustc rustfmt pre-commit rustPackages.clippy rust-analyzer nix-linter nixpkgs-fmt yamllint python310Packages.pre-commit-hooks ];
          RUST_SRC_PATH = rustPlatform.rustLibSrc;
        };
      });
}
