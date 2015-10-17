let
    common-config = {
        # Run GC at 8 am. If it breaks someone will be awake.
        nix.gc.automatic = true;
        nix.gc.dates = "08:00";
        nix.gc.options = "--delete-older-than 7d";

        # Enable a basic firewall
        networking.firewall.enable = true;
        networking.firewall.allowedTCPPorts = [ 22 80 443 ];
        networking.firewall.allowPing = true;

        services.journald.extraConfig = "SystemMaxUse=100M";

        # Linux really likes to swap out things that are user-facing
        # and latency sensitive. Make that less likely:
        boot.kernel.sysctl."vm.swappiness" = 0;
    };
in
{
    network.enableRollback = true;
    network.description = "haverer.jml.io";

    # XXX: Would be nice to put domain names & key package as top-level
    # parameters of physical & logical.

    # TODO: Obviously need a way of installing the actual haverer binary and
    # running it with appropriate settings.

    webserver = { config, pkgs, nodes, ... }:
        let
            # XXX: Can I move this up to top-level let?
            jml_certs = import ../../../jml.io { stdenv = pkgs.stdenv; };
        in (common-config // {

        environment.systemPackages = [ pkgs.nixops pkgs.vim ];

        services.nginx.enable = true;
        services.nginx.config = import ./nginx-config.nix {
            inherit pkgs;
            # XXX: Would like to have these in some place so they're not
            # duplicated in the physical config.
            domain_name = "haverer.jml.io";
            static_domain_name = "static.jml.io";
            static_location = "/haverer";

            ssl_key_path = "${jml_certs}/jml.io.key";
            ssl_certificate_path = "${jml_certs}/jml.io.crt";
            ssl_ca_bundle_path = "${jml_certs}/intermediate_bundle.crt";

            static_file_path = "/home/haverer-api/static/";
            app_server_port = 3000;
        };
    });
}
