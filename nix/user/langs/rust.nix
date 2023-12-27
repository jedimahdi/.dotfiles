{ pkgs, ... }:

{
  home.packages = with pkgs; [
    rust-bin.stable.latest.default
    rust-analyzer
  ];
}
