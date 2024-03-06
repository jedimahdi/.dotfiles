{ username, ... }:
{
  programs.ssh = {
    enable = true;

    matchBlocks = {
      github = {
        hostname = "github.com";
        identityFile = "/home/${username}/.ssh/id_rsa";
      };
      gitlab = {
        hostname = "gitlab.com";
        identityFile = "/home/${username}/.ssh/id_rsa";
      };
    };
  };
}
