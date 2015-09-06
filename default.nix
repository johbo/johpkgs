{ system ? builtins.currentSystem }:


let

  pkgs = import <nixpkgs> {inherit system; };

  inherit(pkgs)
    emacs
    fetchFromGitHub
    fetchurl
    lib
    stdenv
    texinfo;

  callPackage = pkgs.lib.callPackageWith (pkgs // self);

  self = rec {

    johbo-config = callPackage ./config { };

    johbo-common = pkgs.buildEnv {
      name = "johbo-common";

      paths = [
        emacs
        pkgs.emacsPackagesNg.flycheck
        pkgs.emacsPackagesNg.projectile
        emacsPackages.autoComplete
        emacsPackages.d
        emacsPackages.flx-ido
        emacsPackages.jedi
        # TODO: Find out why jedi does not full this in automatically
        emacsPackages.jedi-epcserver
        emacsPackages.yaml

        pkgs.aspell
        pkgs.aspellDicts.de
        pkgs.aspellDicts.en

        pkgs.coreutils
        pkgs.git
        pkgs.mercurial
        pkgs.pstree
        pkgs.tmux
        pkgs.tree
        pkgs.watchman
        # pkgs.xournal

        pkgs.nix-repl
        pkgs.nix-serve
        pkgs.nixops

        pkgs.pylint
        # TODO: priority
        # pkgs.python2
        pkgs.python2Packages.flake8
        pkgs.python2Packages.jedi
        pkgs.python2Packages.pip
        pkgs.python2Packages.supervisor

        pkgs.python3
        pkgs.python3Packages.hovercraft

        pkgs.pythonDocs.html.python27

      ];

      passthru = {
        # Avoiding conflicts by using a low priority for the collection
        meta.priority = 10;
      };
    };

    daemon-fg = callPackage ./daemon-fg { };

    dub = callPackage ./dub { };

    emacsPackages = import ./emacs-modes {
      inherit
        callPackage
        fetchFromGitHub
        epc;
      inherit (pkgs.pythonPackages)
        buildPythonPackage
        jedi
        argparse;
      melpaBuild = import <nixpkgs/pkgs/build-support/emacs/melpa.nix> {
        inherit lib stdenv fetchurl emacs texinfo;
      };
    };

    epc = pkgs.lib.overrideDerivation pkgs.pythonPackages.epc (oldAttr: rec {
      name = "epc-0.0.4";
      src = pkgs.fetchurl {
        url = "http://pypi.python.org/packages/source/e/epc/${name}.tar.gz";
        md5 = "9b91654edf64a5e841f64243b5251eed";
      };
    });

  };
in
self
