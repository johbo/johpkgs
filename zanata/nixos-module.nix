{ config, lib, pkgs, ... }:

with lib;

let

  cfg = config.services.zanata;

  zanata = import ./default.nix {
    inherit (pkgs) stdenv fetchurl jdk8 unzip;
  };

  zanataService = pkgs.stdenv.mkDerivation {
    name = "zanata-server";
    builder = ./nixos-builder.sh;
    inherit zanata;
    inherit (pkgs) su;
    inherit (cfg) tempDir logDir libUrl deployDir serverDir user useJK;
  };

in

{

  ###### interface

  options = {

    services.zanata = {

      enable = mkOption {
        default = false;
        description = "Whether to enable Zanata";
      };

      tempDir = mkOption {
        default = "/tmp";
        description = "Location where Zanata stores its temp files";
      };

      logDir = mkOption {
        default = "/var/log/zanata";
        description = "Location of the logfile directory of Zanata";
      };

      serverDir = mkOption {
        description = "Location of the server instance files";
        default = "/var/lib/zanata";
      };

      deployDir = mkOption {
        description = "Location of the deployment files";
        default = "/nix/var/nix/profiles/default/server/default/deploy/";
      };

      libUrl = mkOption {
        default = "file:///nix/var/nix/profiles/default/server/default/lib";
        description = "Location where the shared library JARs are stored";
      };

      user = mkOption {
        default = "zanata";
        description = "User under which the Zanata server runs.";
      };

      group = mkOption {
        default = "zanata";
        description = "Group for the Zanata user.";
      };

      useJK = mkOption {
        default = false;
        description = "Whether to use to connector to the Apache HTTP server";
      };

    };

  };


  ###### implementation

  config = mkIf cfg.enable {
    systemd.services.zanata = {
      description = "Zanata server";
      script = "${zanataService}/bin/control start";
      wantedBy = [ "multi-user.target" ];
      preStart = ''
        if [ ! -e ${cfg.serverDir} ]
        then
          echo "Creating Zanata server directory in ${cfg.serverDir}."
          mkdir -p ${cfg.serverDir}
          cd ${cfg.serverDir}
          cp -av ${zanata}/standalone/* .

          # Make files accessible for the server user
          chown -R ${cfg.user}: ${cfg.serverDir}
          for i in `find ${cfg.serverDir} -type d`
          do
            chmod 755 $i
          done
          for i in `find ${cfg.serverDir} -type f`
          do
            chmod 644 $i
          done
        else
          echo "Zanata server directory is already present in ${cfg.serverDir}."
        fi
      '';
    };

    users.users.${cfg.user} = {
      description = "Zanata server user";
      group = cfg.group;
      home = cfg.serverDir;
    };

    users.groups.${cfg.group} = {};

  };
}
