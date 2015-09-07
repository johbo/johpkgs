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
        pkgs.emacsPackagesNg.flycheck
        pkgs.emacsPackagesNg.magit
        pkgs.emacsPackagesNg.projectile
        emacsPackages.autoComplete
        emacsPackages.d
        emacsPackages.flx-ido
        emacsPackages.jedi
        # TODO: Find out why jedi does not full this in automatically
        emacsPackages.jedi-epcserver
        emacsPackages.monky
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
        epc;
      inherit (pkgs.pythonPackages)
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

  };
in
self
