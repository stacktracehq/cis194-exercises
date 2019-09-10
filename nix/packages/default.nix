{ compiler ? "default"
}:
let
  haskellPackages = compiler: nixpkgs:
    if compiler == "default"
      then nixpkgs.haskellPackages
      else nixpkgs.haskell.packages.${compiler};

  overlay = self: super: {
    haskellPackages = super.haskellPackages.override {
      overrides = hself: hsuper:  {};
    };
  };
in
  import ./nixpkgs {
    overlays = [overlay];
  }
