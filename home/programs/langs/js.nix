{ pkgs, ... }: {
  home.packages = with pkgs; [
    nodejs-18_x
    typescript
    nodePackages.typescript-language-server
    prettierd
    nodePackages.prettier
    # nodePackages.vscode-langservers-extracted
  ];
}
