{ pkgs ? import ./nix/nixpkgs-pinned.nix }:

pkgs.mkShell {
  name = "gzc-shell";
  version = "0.0.1";

  buildInputs = with pkgs; [
    stack

    haskellPackages.hoogle
  ];
}
