{ stdenv, pkgs }:

let
  home = builtins.getEnv "HOME";

in
stdenv.mkDerivation {
  name = "johbo-configuration";
  src = ./.;
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
    cp -rv etc-darwin/supervisor.d $out/etc
  '' + pkgs.lib.optionalString stdenv.isLinux ''
    cp -rv etc-linux/supervisor.d $out/etc
  '';
}
