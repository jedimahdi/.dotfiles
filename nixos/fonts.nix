{ pkgs, ... }: {
  fonts = {
    enableDefaultPackages = false;
    fontDir.enable = true;
    packages = with pkgs; [
      noto-fonts
      noto-fonts-emoji
      vazir-fonts
      (nerdfonts.override {
        fonts = [
          "VictorMono"
          "DejaVuSansMono"
        ];
      })
    ];
    fontconfig = {
      subpixel.rgba = "rgb";
      defaultFonts = {
        serif = [ "Noto Serif" "Vazirmatn" "Noto Color Emoji" ];
        sansSerif = [ "Noto Sans" "Vazirmatn" "Noto Color Emoji" ];
        monospace = [ "DejaVuSansM Nerd Font" "VictorMono Nerd Font" "Vazirmatn" "Noto Color Emoji" ];
        emoji = [ "Noto Color Emoji" ];
      };
    };
  };
}
