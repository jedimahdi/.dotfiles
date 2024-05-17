{ pkgs, ... }: {
  home.packages = with pkgs; [
    cargo
    cargo-watch
    rustfmt
    rust-analyzer
    rustc
    (writeScriptBin "rust-doc" ''
      #! ${stdenv.shell} -e
      exec firefox "${rustc.doc}/share/doc/rust/html/index.html"
    '')
  ];
}
