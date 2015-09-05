{ stdenv, pkgs }:

let
  home = builtins.getEnv "HOME";

in
stdenv.mkDerivation {
  name = "johbo-configuration";
  src = ./.;

  # TODO: Has a few dependencies:
  # - daemon-fg
  # - fix-symlinks (kind of)
  # - supervisord

  buildPhase = ''
    echo "nothing to build"
  '';
  installPhase = ''
    mkdir $out
    cp -rv bin $out/bin
    cp -rv etc $out/etc

    # Inject home directory into the supervisor configuration
    substituteInPlace $out/etc/supervisord.conf \
      --replace "%(ENV_HOME)s" ${home}

    cp -rv Library $out/Library
  '' + pkgs.lib.optionalString stdenv.isDarwin ''
    # Copy in darwin specific configuration files
    cp -rv etc-darwin/* $out/etc
  '' + pkgs.lib.optionalString stdenv.isLinux ''
    # Copy in linux specific configuration files
    cp -rv etc-linux/* $out/etc

    # TODO: This is not yet cool, should better point into the store to
    # the correct supervisord version. On the other side, I want to use
    # the installed one.
    substituteInPlace $out/etc/systemd/user/supervisord.service \
        --replace "{{HOME}}" ${builtins.getEnv "HOME"}
  '';
}
