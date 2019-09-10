{ stdenv
, fetchFromGitHub
}:
stdenv.mkDerivation rec {
  version = "0.5.1";
  name = "tmux-up-${version}";

  src = fetchFromGitHub {
    owner = "jamesottaway";
    repo = "tmux-up";
    rev = "87a89989ad4a588eb5f02cc2cda50335d26d8435";
    sha256 = "1ykxqayz7qh7c7xzz01wkcvw4hmxg1nr50a6lpr0w0w99qcz0y8x";
  };

  installPhase = ''
    mkdir -p $out/bin
    cp ./tmux-up $out/bin/
  '';
}
