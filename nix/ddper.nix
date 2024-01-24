{
  stdenv,
  lib,
  fetchzip,
  freetype,
  libpng,
  SDL2,
  ffmpeg_5,
  opusfile,
  vulkan-loader,
  libglvnd,
  libnotify,
  glib,
  curl,
  sqlite,
  unzip,
  patchelf,
  makeWrapper,
}:
stdenv.mkDerivation rec {
  name = "ddper";
  version = "7.0";

  src = fetchzip {
    url = "https://ddper.ir/download/DDPER-v${version}-linux.zip";
    hash = "sha256-5GCnmL7ej7o3ZeEITlMHUd1CJ2JtStZEHuWDIKDG/tY=";
  };
  sourceRoot = ".";
  # unpackCmd = "unzip DDPER-v7.0-linux.zip";

  dontConfigure = true;
  dontBuild = true;
  nativeBuildInputs = [
    unzip
    patchelf
    makeWrapper
  ];

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin
    cp -R * $out/
    chmod +x $out/source/DDPER
    # symlink the binary to bin/
    makeWrapper $out/source/DDPER $out/bin/ddper --run "cd $out/source"
    # ln -s $out/source/DDPER $out/bin/ddper

    runHook postInstall
  '';
  preFixup = let
    # we prepare our library path in the let clause to avoid it become part of the input of mkDerivation
    libPath = lib.makeLibraryPath [
      stdenv.cc.cc
      freetype
      libpng
      SDL2
      ffmpeg_5
      opusfile
      vulkan-loader
      libglvnd
      libnotify
      glib
      curl
      sqlite
    ];
  in ''
    patchelf \
      --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" \
      --set-rpath "${libPath}" \
      $out/source/DDPER
  '';

  meta = with lib; {
    homepage = "https://github.com/ddnet/ddnet";
    description = "DDPER Game";
    license = licenses.free;
    platforms = platforms.linux;
  };
}
