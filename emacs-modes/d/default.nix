{stdenv, fetchurl, emacs}:

let version = "20151228";

in stdenv.mkDerivation {
  name = "emacs-d-${version}";

  # TODO : 5501b77a1e212e27dd78e8c0e86424064b439cbb
  src = fetchurl {
    url = "https://github.com/Emacs-D-Mode-Maintainers/Emacs-D-Mode/archive/5501b77a1e212e27dd78e8c0e86424064b439cbb.tar.gz";
    sha256 = "1zb6ci6abnvdhrv5r88fjgyjvca3w2277vk1skx5sjcq9pccch50";
  };

  buildInputs = [ emacs ];

  buildPhase = ''
    emacs -L . --batch -f batch-byte-compile *.el
  '';

  installPhase = ''
    install -d $out/share/emacs/site-lisp
    install *.el *.elc $out/share/emacs/site-lisp
  '';
}
