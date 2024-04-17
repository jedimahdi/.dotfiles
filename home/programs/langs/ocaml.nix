{ pkgs, ... }: {
  home.packages = with pkgs; [
    opam
  ];
}
