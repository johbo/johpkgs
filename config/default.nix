{ stdenv }:

stdenv.mkDerivation {
  name = "johbo-configuration";
  src = ./.;
  buildPhase = ''
    echo "nothing to build"
  '';
  installPhase = ''
    mkdir $out
    cp -rv etc $out/etc
  '';
}
