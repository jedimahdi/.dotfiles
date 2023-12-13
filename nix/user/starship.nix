{ pkgs, lib, ... }:

with lib;
with builtins;

let
  isRustFile = path: type:
    hasSuffix ".rs" path && type == "regular" && path != "mod.rs";
  mergeAllAttrSets = attrsSets:
    foldl' (recursiveUpdate) { } attrsSets;
  disableModules = isDisabled: modules:
    mergeAllAttrSets (map (mod: { "${mod}".disabled = isDisabled; }) modules);

  starshipPackage = pkgs.starship;
  promptOrder = [
    "directory"
    "git_branch"
    "git_commit"
    "git_state"
    "git_metrics"
    "git_status"
    "line_break"
    "nix_shell"
    "rust"
    "purescript"
    "haskell"
    "nodejs"
    "character"
  ];
  promptFormat = concatStrings (map (s: "\$${s}") promptOrder);
  modulesSources = readDir "${starshipPackage.src}/src/modules";
  enabledModules = disableModules false promptOrder; # <== enabled all modules used in the prompt are enabled
  disabledModules = pipe modulesSources [
    # <== from starship's sources...
    (filterAttrs isRustFile) # <== keep only Rust files...
    attrNames # <== get the filenames...
    (map (removeSuffix ".rs")) # <== remove Rust source extension...
    (filter (x: x != "custom")) # <== remove custom module...
    (subtractLists promptOrder) # <== do not disable modules used in the prompt...
    (disableModules true) # <== and finally build the configuration
  ];
in
{
  programs.starship = {
    enable = true;
    package = starshipPackage;
    enableZshIntegration = true;
    settings = mergeAllAttrSets [
      enabledModules
      disabledModules
    ];
  };
}
