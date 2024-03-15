{ ... }:
{
  programs.zoxide = {
    enable = true;
    enableZshIntegration = true;
    enableNushellIntegration = false;
    enableBashIntegration = false;
    enableFishIntegration = false;
    options = [ "--cmd" "cd" ];
  };
}
