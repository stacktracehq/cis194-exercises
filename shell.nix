{ nixpkgs ? import ./nixpkgs.nix {}
}:
let
  package = import ./. {
    inherit nixpkgs;
  };
  withTools = nixpkgs.haskell.lib.addBuildDepends
    package
    [
      nixpkgs.haskellPackages.cabal-install
      nixpkgs.haskellPackages.ghcid
      nixpkgs.haskellPackages.hindent
      nixpkgs.haskellPackages.hlint
    ];
in
  withTools.env
