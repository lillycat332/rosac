{ nixpkgs ? import <nixpkgs> { }, compiler ? "ghc924" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./rosalia.nix { }
