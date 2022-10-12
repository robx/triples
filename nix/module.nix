{
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}:
with lib; let
  cfg = config.services.triples;
in {
  options.services.triples = {
    enable = mkEnableOption "triples server";

    user = mkOption {
      type = types.str;
      default = "triples";
      description = lib.mdDoc "User account under which triples runs";
    };

    hostName = mkOption {
      type = types.str;
      default = "localhost";
      description = "Hostname to serve triples on";
    };

    nginx = mkOption {
      type = types.submodule
        (import "${modulesPath}/services/web-servers/nginx/vhost-options.nix" {
          inherit config lib;
        });
      default = {};
      description = "Extra configuration for the nginx virtual host of triples.";
    };
  };

  config = mkIf cfg.enable {
    users.users.triples = mkIf (cfg.user == "triples") {
      name = "triples";
      isSystemUser = true;
      group = "triples";
      description = "triples service user";
    };
    users.groups.triples = mkIf (cfg.user == "triples") {};

    systemd.services.triples = {
      description = "Run triples backend";
      wantedBy = ["multi-user.target"];
      after = ["networking.target"];
      serviceConfig = {
        User = "triples";
        ExecStart = "${pkgs.triples-serve}/bin/serve -listen=localhost:8080 -bot=false -static=${pkgs.triples-static}/";
        Restart = "always";
      };
    };

    services.nginx = {
      upstreams.triples-backend.servers."localhost:8080" = {};
      virtualHosts."${cfg.hostName}" = mkMerge [
        cfg.nginx
        {
          locations = {
            "/api/join" = {
              proxyWebsockets = true;
              proxyPass = "http://triples-backend/api/join";
              # to make this work with host:port, switch to
              # $http_host (but see nix nginx config checks)
              extraConfig = ''
                # go websocket origin check
                proxy_set_header Host $host;
              '';
            };
            "/" = {
              proxyPass = "http://triples-backend/";
            };
          };
        }
      ];
    };
  };
}
