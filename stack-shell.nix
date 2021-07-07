{
  pkgs ? import ./nix/nixpkgs-pinned.nix
}:

pkgs.haskell.lib.buildStackProject {
  nativeBuildInputs = with pkgs; [

  ];

  name = "zilch-shell";
}
