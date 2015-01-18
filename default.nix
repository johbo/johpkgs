{ system ? builtins.currentSystem }:


let

  pkgs = import <nixpkgs> {inherit system; };

  callPackage = pkgs.lib.callPackageWith (pkgs // self);

  self = rec {

    dub = callPackage ./dub { };

    emacsPackages = {
      d = callPackage ./emacs-modes/d { };
    };

  };
in
self
