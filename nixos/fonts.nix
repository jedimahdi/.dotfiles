{ pkgs, ... }: {
  fonts = {
    enableDefaultPackages = false;
    fontDir.enable = true;
    packages = with pkgs; [
      noto-fonts
      noto-fonts-emoji
      vazir-fonts
      victor-mono
      jetbrains-mono
      (nerdfonts.override {
        fonts = [
          "VictorMono"
          "JetBrainsMono"
        ];
      })
    ];
    fontconfig = {
      subpixel.rgba = "rgb";
      defaultFonts = {
        serif = [ "Noto Serif" "Vazirmatn" "Noto Color Emoji" ];
        sansSerif = [ "Noto Sans" "Vazirmatn" "Noto Color Emoji" ];
        monospace = [ "JetBrainsMono NF" "VictorMono Nerd Font" "Vazirmatn" "Noto Color Emoji" ];
        emoji = [ "Noto Color Emoji" ];
      };
    };
  };
}
