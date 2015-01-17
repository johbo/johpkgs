with import <nixpkgs> {};

rec {

  dub = callPackage ./dub { };

  emacsPackages = {
    d = callPackage ./emacs-modes/d { };
  };

}
