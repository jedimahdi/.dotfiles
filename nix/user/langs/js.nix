{ pkgs, ... }:

{
  home.packages = with pkgs; [
    nodejs-18_x
    typescript
    nodePackages.typescript-language-server
    nodePackages.prettier
    nodePackages.vscode-langservers-extracted
  ];
}
