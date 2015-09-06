{ system ? builtins.currentSystem }:


let

  pkgs = import <nixpkgs> {inherit system; };

  callPackage = pkgs.lib.callPackageWith (pkgs // self);

  self = rec {

    johbo-config = callPackage ./config { };

    johbo-common = pkgs.buildEnv {
      name = "johbo-common";

      paths = [
        pkgs.emacs
        pkgs.emacsPackages.autoComplete
        pkgs.emacsPackagesNg.flycheck
        pkgs.emacsPackagesNg.projectile
        emacsPackages.d
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

    emacsPackages = callPackage ./emacs-modes { };

  };
in
self
