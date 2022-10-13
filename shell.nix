{ nixpkgs ? import <nixpkgs> { }, compiler ? "ghc924" }:
(import ./default.nix { inherit nixpkgs compiler; }).env
