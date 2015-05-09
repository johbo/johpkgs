{ system ? builtins.currentSystem }:


let

  pkgs = import <nixpkgs> {inherit system; };

  callPackage = pkgs.lib.callPackageWith (pkgs // self);

  self = rec {

    johbo-config = callPackage ./config { };

    johbo-common = pkgs.buildEnv {
      name = "johbo-common";

      paths = [
        johbo-config

        emacs
        emacsPackages.d
        emacsPackages.yaml

        pkgs.aspell
        pkgs.aspellDicts.de
        pkgs.aspellDicts.en

        pkgs.coreutils
        pkgs.git
        pkgs.mercurial
        pkgs.screen
        pkgs.tmux
        pkgs.tree
        pkgs.watchman
        # pkgs.xournal

        pkgs.nix-repl
        pkgs.nix-serve

        pkgs.pythonDocs.html.python27

      ];

      passthru = {
        # Avoiding conflicts by using a low priority for the collection
        meta.priority = 10;
      };
    };

    dub = callPackage ./dub { };

    emacs = pkgs.lib.overrideDerivation pkgs.emacs (oldAttrs: {
      # Adding support for the GUI integration for darwin
      configureFlags = oldAttrs.configureFlags ++ pkgs.lib.optional pkgs.stdenv.isDarwin [
        "--with-ns --disable-ns-self-contained"
      ];

      # Copy over the generated Emacs.app data
      postInstall = oldAttrs.postInstall + pkgs.lib.optionalString pkgs.stdenv.isDarwin ''
        mkdir -p $out/Applications
        cp -r nextstep/Emacs.app $out/Applications
      '';

    });

    emacsPackages = {
      d = callPackage ./emacs-modes/d { };
      yaml = callPackage ./emacs-modes/yaml { };
    };

  };
in
self
