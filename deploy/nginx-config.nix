{ pkgs
, domain_name
, static_domain_name
, static_location
, static_file_path
, ssl_ca_bundle_path
, ssl_certificate_path
, ssl_key_path
, app_server_port
, http_port ? 80
, https_port ? 443
}: ''

events { }

http {
    server {
        listen          ${toString http_port};
        server_name     _;
        return          301 https://$host$request_uri;
    }

    server {
        listen          ${toString https_port} ssl spdy;
        server_name     ${domain_name};

        ssl_certificate ${ssl_certificate_path};
        ssl_certificate_key ${ssl_key_path};
        ssl_protocols TLSv1.2 TLSv1.1 TLSv1;
        add_header Strict-Transport-Security "max-age=31536000; includeSubdomains";

        ssl_stapling on;
        ssl_stapling_verify on;
        ssl_trusted_certificate ${ssl_ca_bundle_path};

        location / {
            proxy_pass http://127.0.0.1:${toString app_server_port};
        }
    }

    server {
        listen ${toString https_port} ssl spdy;
        server_name ${static_domain_name};

        ssl_certificate ${ssl_certificate_path};
        ssl_certificate_key ${ssl_key_path};
        ssl_protocols TLSv1.2 TLSv1.1 TLSv1;
        add_header Strict-Transport-Security "max-age=31536000; includeSubdomains";

        ssl_stapling on;
        ssl_stapling_verify on;
        ssl_trusted_certificate ${ssl_ca_bundle_path};

        location ${static_location} {
            alias ${static_file_path};
            expires max;
        }
    }
}
''
