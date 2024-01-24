{
  pkgs,
  name,
}:
pkgs.stable.mkShell {
  inherit name;

  nativeBuildInputs = builtins.concatMap builtins.attrValues [
    ###################################################
    # Code styles:
    {
      inherit (pkgs.stable) pre-commit nixpkgs-fmt nix-linter purs-tidy shfmt;
      inherit (pkgs.unstable.python3Packages) pre-commit-hooks yamllint;
      inherit (pkgs.unstable.nodePackages) prettier;
    }

    ###################################################
    # Languages:
    {
      inherit (pkgs.stable) nodejs-16_x;
      inherit (pkgs.unstable.nodePackages) typescript;
      inherit (pkgs.stable) purescript;
    }

    ###################################################
    # Command line tools:
    {
      inherit (pkgs.stable) gitFull gitflow;
      inherit (pkgs.unstable.nodePackages) parcel-bundler;
    }

    ###################################################
    # Language servers:
    {
      inherit (pkgs.stable) dhall-lsp-server;
      inherit
        (pkgs.unstable.nodePackages)
        bash-language-server
        typescript-language-server
        purescript-language-server
        vscode-json-languageserver-bin
        vscode-html-languageserver-bin
        yaml-language-server
        ;
    }

    ###################################################
    # Package managers:
    {
      inherit (pkgs.stable) spago pulp;
      inherit (pkgs.unstable.nodePackages) bower;
    }
  ];
}
