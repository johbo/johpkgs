{ callPackage
, fetchFromGitHub
, melpaBuild
}:

rec {
  d = callPackage ./d { };

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

  yaml = callPackage ./yaml { };
}
