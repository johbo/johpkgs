{ system ? builtins.currentSystem }:


let

  pkgs = import <nixpkgs> {inherit system; };

  callPackage = pkgs.lib.callPackageWith (pkgs // self);

  self = rec {

    dub = callPackage ./dub { };

    emacs = pkgs.lib.overrideDerivation pkgs.emacs (oldAttrs: {
      # Adding support for the GUI integration for darwin
      configureFlags = oldAttrs.configureFlags ++ pkgs.lib.optional pkgs.stdenv.isDarwin [
        "--with-ns --disable-ns-self-contained"
      ];

      postInstall = oldAttrs.postInstall + pkgs.lib.optionalString pkgs.stdenv.isDarwin ''
        # Copy over the generated Emacs.app data
        mkdir -p $out/Applications
        cp -r nextstep/Emacs.app $out/Applications
      '';

    });

    emacsPackages = {
      d = callPackage ./emacs-modes/d { };
    };

  };
in
self
