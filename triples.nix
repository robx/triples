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

    nginx = mkOption {
      type = types.submodule (import "${modulesPath}/services/web-servers/nginx/vhost-options.nix" {inherit config lib;});
      default = {};
      description = "Extra configuration for the nginx virtual host of triples.";
    };
  };

  config = mkIf cfg.enable {
    systemd.services.triples = {
      description = "Run triples backend";
      wantedBy = ["multi-user.target"];
      after = ["networking.target"];
      serviceConfig = {
        ExecStart = "${pkgs.triples-serve}/bin/serve -bot=false -static=${pkgs.triples-static}/";
        Restart = "always";
      };
    };
    services.nginx = {
      enable = true;
      upstreams.triples-backend.servers."localhost:8080" = {};
      virtualHosts."localhost" = mkMerge [
        cfg.nginx
        {
          #          default = true;
          # root = mkForce "${cfg.package}/share/fluidd/htdocs";
          locations = {
            "/api/join" = {
              proxyWebsockets = true;
              proxyPass = "http://triples-backend/api/join";
              extraConfig = ''
                # go websocket origin check
                # ($http_host instead of $host because the latter strips
                # port number, relevant for local vm test)
                proxy_set_header Host $http_host;
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
