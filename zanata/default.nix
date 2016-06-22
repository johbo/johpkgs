{ stdenv, fetchurl, jdk8, unzip }:

let
  zanata-src = fetchurl {
    url = https://github.com/zanata/zanata-server/releases/download/server-3.9.0/zanata-3.9.0-wildfly.zip;
    sha256 = "16djdj4fh5qvvci6cfhw232imswnw6kqqjqqkgflm7s7r02hbdw1";
  };

  wildfly-src = fetchurl {
    url = http://download.jboss.org/wildfly/10.0.0.Final/wildfly-10.0.0.Final.tar.gz;
    sha256 = "0jx8435v404cfqp262qxg7raa3z43g4h0xiyd44srmxda944w370";
  };

in stdenv.mkDerivation {
  name = "zanata-3.9.0";

  src = wildfly-src;
  buildInputs = [ unzip ];

  phases = [ "unpackPhase" "installPhase" "fixupPhase" ];

  installPhase = ''
    mv $PWD $out
    find $out/bin -name \*.sh -print0 | xargs -0 sed -i -e '/#!\/bin\/sh/aJAVA_HOME=${jdk8}'

    # The installation idea is to extract it on top of jboss,
    # see http://docs.zanata.org/en/release/user-guide/system-admin/configuration/installation/
    unzip ${zanata-src} -d $out
  '';

}
