{ compiler ? "default"
}:
let
  packages = import ./nix/packages {
    inherit compiler;
  };

  tools = import ./nix/tools.nix packages;

  package = import ./. {
    inherit compiler;
  };
in
  (packages.haskell.lib.addBuildDepends package tools).env
