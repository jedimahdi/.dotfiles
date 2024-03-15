{ ... }:
let
  bashIntegration = ''
    function ya() {
      local tmp="$(mktemp -t "yazi-cwd.XXXXX")"
      yazi "$@" --cwd-file="$tmp"
      if cwd="$(cat -- "$tmp")" && [ -n "$cwd" ] && [ "$cwd" != "$PWD" ]; then
        cd "$cwd"
      fi
      rm -f -- "$tmp"
    }
  '';
in
{
  programs.yazi = {
    enable = true;
    enableZshIntegration = false;
  };
  programs.zsh.initExtra = bashIntegration;
}
