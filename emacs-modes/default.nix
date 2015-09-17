{ callPackage
, fetchFromGitHub
, melpaBuild
, buildPythonPackage
, jedi
, epc
, argparse
}:

let

  jedi_ = jedi;
  epc_ = epc;
  argparse_ = argparse;

in rec {

  autoComplete = melpaBuild rec {
    version = "1.5.0";
    pname = "auto-complete";

    src = fetchFromGitHub {
      owner = "auto-complete";
      repo = "auto-complete";
      rev = "v" + version;
      sha256 = "0q880dksf3bj1hixa4zhj3ybvrqf1wcnjnmb37i49qwdy6p1ma06";
    };
    packageRequires = [ popup ];
    meta = {
      description = "Emacs auto-complete package";
    };
  };

  # TODO: Something odd with 0.3.2, seems that they have not updated
  # concurrent.el
  concurrent = melpaBuild rec {
    version = "0.3.1";
    pname = "concurrent";

    src = fetchFromGitHub {
      owner = "kiwanami";
      repo = "emacs-deferred";
      rev = "v" + version;
      sha256 = "1ykibqjxbbv271s19ipw3r289y6w47ic8sji2bc0p5lfas9p582g";
    };
    packageRequires = [ deferred ];
    files = [ "concurrent.el" ];
    meta = {
      description = "'concurrent.el' is a higher level library for concurrent tasks.";
    };
  };

  ctable = melpaBuild rec {
    version = "0.1.2";
    pname = "ctable";

    src = fetchFromGitHub {
      owner = "kiwanami";
      repo = "emacs-ctable";
      rev = version;
      sha256 = "13zq8kym1y6bzrpxbcdz32323a6azy5px4ridff6xh8bfprwlay3";
    };
    meta = {
      description = "Table component for Emacs Lisp";
    };
  };

  deferred = melpaBuild rec {
    version = "0.3.1";
    pname = "deferred";

    src = fetchFromGitHub {
      owner = "kiwanami";
      repo = "emacs-${pname}";
      rev = "v" + version;
      sha256 = "1ykibqjxbbv271s19ipw3r289y6w47ic8sji2bc0p5lfas9p582g";
    };
    files = [ "deferred.el" ];
    meta = {
      description = "Simple asynchronous functions for emacs-lisp";
    };
  };

  epc = melpaBuild rec {
    pname = "epc";
    version = "0.1.1";
    src = fetchFromGitHub {
      owner = "kiwanami";
      repo = "emacs-epc";
      rev = "dbae585622fa7b556945cc7475f354976d26b065";
      sha256 = "1ayhknqv1wmar2j2r4n7nqcp69i7k222qx524x4wv61ys324d004";
    };
    packageRequires = [ concurrent ctable ];
    meta = {
      description = "A RPC stack for Emacs Lisp";
    };
  };


  d = callPackage ./d { };

  feature-mode = melpaBuild rec {
    pname = "feature-mode";
    version = "0.4";
    src = fetchFromGitHub {
      owner = "michaelklishin";
      repo = "cucumber.el";
      rev = "40886bc4cc5b1e855d6bb78505ebc651593d409d";
      sha256 = "18b27xfajhd0vi7dcaky7pi7vmnlmdb88gkx3hwangj78d24as5d";
    };
    meta = {
      description = " Emacs mode for editing Cucumber plain text stories";
    };
  };

  flx = melpaBuild rec {
    pname = "flx";
    version = "0.6-pre";
    src = fetchFromGitHub {
      owner = "lewang";
      repo = "flx";
      rev = "10db5313698068685ecc9bacee86973e53115e10";
      sha256 = "0l7sdx056saf920fd6c94a9ri2m8za7wjiq09iqakfnhax2c9aq1";
    };
    files = [ "flx.el" ];
    meta = {
      description = "Fuzzy matching for Emacs - Algorithm";
    };
  };

  flx-ido = melpaBuild rec {
    pname = "flx-ido";
    version = "0.6-pre";
    src = fetchFromGitHub {
      owner = "lewang";
      repo = "flx";
      rev = "10db5313698068685ecc9bacee86973e53115e10";
      sha256 = "0l7sdx056saf920fd6c94a9ri2m8za7wjiq09iqakfnhax2c9aq1";
    };
    files = [ "flx-ido.el" ];
    packageRequires = [ flx ];
    meta = {
      description = "Fuzzy matching for Emacs - IDO integration";
    };
  };

  jedi = melpaBuild rec {
    pname = "jedi";
    version = "0.2.5";
    src = fetchFromGitHub {
      owner = "tkf";
      repo = "emacs-jedi";
      rev = "v" + version;
      sha256 = "08maylbnpz7hg3lz53hp2n4xd7xazkg51xhz1qf388bgz36bvj0q";
    };
    files = [ "jedi.el" ];
    packageRequires = [ jedi-core ];
    propagatedBuildInputs = [ jedi-epcserver ];
    meta = {
      description = "Python auto-completion for Emacs";
    };
  };

  jedi-core = melpaBuild rec {
    pname = "jedi-core";
    version = "0.2.5";
    src = fetchFromGitHub {
      owner = "tkf";
      repo = "emacs-jedi";
      rev = "v" + version;
      sha256 = "08maylbnpz7hg3lz53hp2n4xd7xazkg51xhz1qf388bgz36bvj0q";
    };
    files = [ "jedi-core.el" ];
    packageRequires = [ autoComplete epc python-environment ];
    meta = {
      description = "Python auto-completion for Emacs - Core.";
    };
  };

  jedi-epcserver = buildPythonPackage rec {
    version = "0.2.5";
    name = "jediepcserver-${version}";
    src = fetchFromGitHub {
      owner = "tkf";
      repo = "emacs-jedi";
      rev = "v" + version;
      sha256 = "08maylbnpz7hg3lz53hp2n4xd7xazkg51xhz1qf388bgz36bvj0q";
    };
    propagatedBuildInputs = [
      argparse_
      epc_
      jedi_
    ];

  };

  monky = melpaBuild rec {
    version = "0.1";
    pname = "monky";

    src = fetchFromGitHub {
      owner = "ananthakumaran";
      repo = "monky";
      rev = "48c0200910739b6521f26f6423b2bfb8c38b4482";
      sha256 = "0ddkcb5rzpcqpsrwkhvm9kzpx2mlrrsp7psljkz5q5qfvy3wdagh";
    };
    files = [ "monky.el" "monky.info" ];
    meta = {
      description = "Magit for Hg";
    };
  };

  popup = melpaBuild rec {
    version = "0.5.3";
    pname = "popup";

    src = fetchFromGitHub {
      owner = "auto-complete";
      repo = "popup-el";
      rev = "v" + version;
      sha256 = "1y538siabcf1n00wr4iz5gbxfndw661kx2mn9w1g4lg7yi4n0h0h";
    };
    meta = {
      description = " Visual Popup Interface Library for Emacs";
    };
  };

  python-environment = melpaBuild rec {
    pname = "python-environment";
    version = "0.0.2";
    src = fetchFromGitHub {
      owner = "tkf";
      repo = "emacs-python-environment";
      rev = version;
      sha256 = "0q6bib9nr6xiq6npzbngyfcjk87yyvwzq1zirr3z1h5wadm34lsk";
    };
    files = [ "python-environment.el" ];
    packageRequires = [ epc ];
    meta = {
      description = "Python virtualenv API for Emacs Lisp";
    };
  };

  sr-speedbar = melpaBuild rec {
    pname = "sr-speedbar";
    version = "0.1.10";
    src = fetchFromGitHub {
      owner = "emacsmirror";
      repo = "sr-speedbar";
      rev = "ecfd2c866c157c3bad44b767feb70ad969926852";
      sha256 = "0bs13s8y77q3wizxvy5d6v368sdcf417dx53zvy4mqknjb20aynh";
    };
    meta = {
      description = "Same frame speedbar";
    };
  };

  yaml = callPackage ./yaml { };

  yasnippet = melpaBuild rec {
    pname = "yasnippet";
    version = "0.8.1";
    src = fetchFromGitHub {
      owner = "capitaomorte";
      repo = "yasnippet";
      rev = "2f6cfef03139dbb74b0f4aeaffef0f9647bee5b8";
      sha256 = "0ki9fqmag7ghmc1kfkh63mah1bhbafi8fcp729q5hzb4c1jgm5nf";
    };
    packageRequires = [ ];
    meta = {
      description = "A template system for Emacs ";
    };
  };

}
