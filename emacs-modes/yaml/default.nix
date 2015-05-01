{stdenv, fetchurl, emacs}:

# Note: Don't have a version, using date as fallback.
let version = "v0.0.11";

in stdenv.mkDerivation {
  name = "emacs-yaml-${version}";

  src = fetchurl {
    url = "https://github.com/yoshiki/yaml-mode/archive/${version}.tar.gz";
    sha256 = "0sjhw891kfq5r991akjzb908hj2c2ai6k958dkyw5wymxjj2c8kd";
  };

  buildInputs = [ emacs ];

  buildPhase = ''
    emacs -L . --batch -f batch-byte-compile *.el
  '';

  installPhase = ''
    install -d $out/share/emacs/site-lisp
    install *.el *.elc $out/share/emacs/site-lisp
  '';

  meta = {
    description = "Major mode for editing YAML files";
    homepage = https://github.com/yoshiki/yaml-mode;
    license = stdenv.lib.licenses.gpl2;
    platforms = stdenv.lib.platforms.all;
  };

}
