{ stdenv, dmd }:

stdenv.mkDerivation rec {
  version = "0.1";
  name = "daemon-fg-${version}";
  src = ./.;
  buildInputs = [
    dmd
  ];

  buildPhase = ''
    dmd daemon-fg.d
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp daemon-fg $out/bin
  '';
}
