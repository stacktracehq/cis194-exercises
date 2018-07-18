{ nixpkgs ? import ./nixpkgs.nix {} }:
nixpkgs.haskellPackages.callPackage ./package.nix {}
