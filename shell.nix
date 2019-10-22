{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.mkGhcidShell ./default.nix { buildInputs = [ pkgs.openjdk ]; }
