{ compiler ? "default"
}:
let
  haskellPackages = compiler: nixpkgs:
    if compiler == "default"
      then nixpkgs.haskellPackages
      else nixpkgs.haskell.packages.${compiler};
in
  import ./nixpkgs {
    config = {
      packageOverrides = nixpkgs: {
        haskellPackages = (haskellPackages compiler nixpkgs).override {
          # We don't have any package overrides as yet
          # This is here as a placeholder, in case we need
          # any.
          overrides = self: super: {};
        };
      };
    };
  }
