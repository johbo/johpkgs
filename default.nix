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

  buildPythonPackage = pkgs.pythonPackages.buildPythonPackage;

  callPackage = pkgs.lib.callPackageWith (pkgs // self);

  self = rec {

    johbo-config = callPackage ./config { };

    johbo-common = pkgs.buildEnv {
      name = "johbo-common";

      paths = [
        emacs

        # completion tools for D
        pkgs.emacsPackagesNg.ac-dcd
        pkgs.dcd

        pkgs.emacsPackagesNg.flycheck
        pkgs.emacsPackagesNg.magit
        pkgs.emacsPackagesNg.multiple-cursors
        pkgs.emacsPackagesNg.projectile
        emacsPackages.autoComplete
        emacsPackages.d
        emacsPackages.feature-mode
        emacsPackages.flx-ido
        emacsPackages.jedi
        # TODO: Find out why jedi does not fill this in automatically
        emacsPackages.jedi-epcserver
        emacsPackages.monky
        emacsPackages.sr-speedbar
        emacsPackages.yaml
        emacsPackages.yasnippet

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
        flake8
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
        buildPythonPackage
        fetchFromGitHub
        epc
        jedi;
      inherit (pkgs.pythonPackages)
        argparse;
      melpaBuild = import <nixpkgs/pkgs/build-support/emacs/melpa.nix> {
        inherit lib stdenv fetchurl emacs texinfo;
      };
    };

    epc = buildPythonPackage rec {
      name = "epc-0.0.5";
      src = pkgs.fetchurl {
        url = "http://pypi.python.org/packages/source/e/epc/${name}.tar.gz";
        md5 = "de54a24ace8a9b3f5b2d8f014b8c4a42";
      };
      propagatedBuildInputs = [ sexpdata ];
      doCheck = false;
    };

    flake8 = pkgs.lib.overrideDerivation pkgs.pythonPackages.flake8 (oldAttr: rec {
      propagatedBuildInputs = oldAttr.propagatedBuildInputs ++ [
        # hacking
        flake8-import-order
      ];
    });

    flake8-import-order = buildPythonPackage rec {
      version = "0.6.1";
      name = "flake8-import-order-${version}";
      src = fetchurl {
        url = "https://pypi.python.org/packages/source/f/flake8-import-order/${name}.tar.gz";
        sha256 = "1qr5mga92ylibn4037165w3vixw6qrhhd9hr1f832b89hm9ln24i";
      };
      # TODO: Check again, was very hard to convince it to run, right now only
      # want the import checker, so it's fine to ignore the rest.
      doCheck = false;
      propagatedBuildInputs = with pkgs.pythonPackages; [
        pep8
      ];
    };

    hacking = buildPythonPackage rec {
      version = "0.10.2";
      name = "hacking-${version}";
      src = fetchurl {
        url = "https://pypi.python.org/packages/source/h/hacking/${name}.tar.gz";
        sha256 = "1a310k3dv04jg7zvmk37h2ql7y9kf4hvdxb74bjlwdxgmy6h4wap";
      };
      # TODO: Check again, was very hard to convince it to run, right now only
      # want the import checker, so it's fine to ignore the rest.
      doCheck = false;
      preBuild = ''
        echo "" > requirements.txt
      '';
      propagatedBuildInputs = with pkgs.pythonPackages; [
        mccabe
        pbr
        six
      ];
    };

    jedi = pkgs.lib.overrideDerivation pkgs.pythonPackages.jedi (oldAttr: rec {
      name = "jedi-0.9.0";
      src = pkgs.fetchurl {
        url = "https://pypi.python.org/packages/source/j/jedi/${name}.tar.gz";
        md5 = "2fee93d273622527ef8c97ac736e92bd";
      };
    });

    sexpdata = pkgs.lib.overrideDerivation pkgs.pythonPackages.sexpdata (oldAttr: rec {
      name = "sexpdata-0.0.3";
      src = pkgs.fetchurl {
        url = "http://pypi.python.org/packages/source/s/sexpdata/${name}.tar.gz";
        md5 = "de9c2c3ee28551e766cb535c0b2cebf0";
      };
    });

  };
in
self
