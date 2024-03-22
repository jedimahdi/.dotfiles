{
  programs.ssh = {
    enable = true;
    matchBlocks =
      let
        commonIdFile = "~/.ssh/id_ed25519";
      in
      {
        github = {
          hostname = "github.com";
          identityFile = commonIdFile;
        };
        gitlab = {
          hostname = "gitlab.com";
          identityFile = commonIdFile;
        };
      };
  };
}
