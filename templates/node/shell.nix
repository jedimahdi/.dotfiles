{ pkgs
, name
,
}:
pkgs.stable.mkShell {
  inherit name;

  nativeBuildInputs = builtins.concatMap builtins.attrValues [
    ###################################################
    # Code styles:
    {
      inherit (pkgs.stable) pre-commit nixpkgs-fmt nix-linter;
      inherit (pkgs.stable.python3Packages) pre-commit-hooks yamllint;
      inherit (pkgs.stable.nodePackages) prettier;
    }

    ###################################################
    # Languages:
    {
      inherit (pkgs.stable) nodejs-16_x;
      inherit (pkgs.unstable.nodePackages) typescript;
    }

    ###################################################
    # Command line tools:
    { inherit (pkgs.stable) gitFull; }

    ###################################################
    # Language servers:
    {
      inherit
        (pkgs.stable.nodePackages)
        vscode-json-languageserver-bin
        typescript-language-server
        yaml-language-server
        ;
    }
  ];
}
