with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "dub-0.9.22";

  src = fetchurl {
    url = "https://github.com/rejectedsoftware/dub/archive/v0.9.22.tar.gz";
    sha256 = "0vhn96ybbsfflldlbyc17rmwb7bz21slbm189k5glyfr9nnp4cir";
  };

  buildInputs = [ unzip curl ];

  propagatedBuildInputs = [ gcc dmd ];

  buildPhase = ''
      ./build.sh
  '';

  installPhase = ''
      mkdir $out
      mkdir $out/bin
      cp bin/dub $out/bin
  '';

  meta = with stdenv.lib; {
    description = "DUB - Build tool for D projects";
    homepage = http://code.dlang.org/;
    license = "TODO";
    platforms = platforms.unix;
  };
}

