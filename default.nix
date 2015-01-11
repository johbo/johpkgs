with import <nixpkgs> {};

rec {

  emacsPackages = {
    d = callPackage ./emacs-modes/d { };
  };

}
