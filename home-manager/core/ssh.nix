{ username, ... }:
{
  programs.ssh = {
    enable = true;

    matchBlocks = {
      github = {
        hostname = "github.com";
        identityFile = "/home/${username}/.ssh/id_ed25519";
      };
      gitlab = {
        hostname = "gitlab.com";
        identityFile = "/home/${username}/.ssh/id_ed25519";
      };
    };
  };
}
