{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    let env = { mkShell, stdenvNoCC, mill }:
      mkShell.override { stdenv = stdenvNoCC; } {
        nativeBuildInputs = [ mill ];
      };
    in flake-utils.lib.eachDefaultSystem (system: {
      devShell = nixpkgs.legacyPackages.${system}.callPackage env {};
    });
}
