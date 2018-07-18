config:
import (builtins.fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/7e7f286fe6af870acb793b02ea5b36d2fb133ace.tar.gz";
  sha256 = "16zmyy845w47mgdp1wwf7brv9sh14rnd7dyf2j4paa2ycs7h0s86";
}) config
