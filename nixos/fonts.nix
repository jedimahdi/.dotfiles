{ pkgs, ... }: {
  fonts = {
    enableDefaultPackages = false;
    fontDir.enable = true;
    packages = with pkgs; [
      noto-fonts
      noto-fonts-emoji
      vazir-fonts
      victor-mono
      (nerdfonts.override {
        fonts = [
          "VictorMono"
        ];
      })
    ];
    fontconfig = {
      subpixel.rgba = "rgb";
      defaultFonts = {
        serif = [ "Noto Serif" "Vazirmatn" "Noto Color Emoji" ];
        sansSerif = [ "Noto Sans" "Vazirmatn" "Noto Color Emoji" ];
        monospace = [ "Victor Mono SemiBold:style=SemiBold" "Victor Mono Medium" "Victor Mono" "VictorMono Nerd Font" "Vazirmatn" "Noto Color Emoji" ];
        emoji = [ "Noto Color Emoji" ];
      };
    };
  };
}
