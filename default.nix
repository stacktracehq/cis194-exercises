{ compiler ? "default"
}:
let
  packages = import ./nix/packages {
    inherit compiler;
  };
in
  packages.haskellPackages.callPackage ./package.nix {}
