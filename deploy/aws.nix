let
    region = "us-west-2";  # Arbitrarily chosen
in
rec {

    /* Physical */
    resources.elasticIPs.webserver = {
        inherit region;
    };

    resources.ec2SecurityGroups.webserver = {
        inherit region;
        name = "webserver"
        description = "Allow web and SSH traffic"
        # XXX: There's currently no way of allowing egress. Would like to
        # allow OCSP responder at 178.255.83.1/32 on port 80:
        # https://github.com/NixOS/nixops/issues/295

        # XXX: Also, generate this list from
        # networking.firewall.allowedTCPPorts
        rules = [
            { fromPort = 22; toPort = 22; sourceIp = "0.0.0.0/0"; }
            { fromPort = 80; toPort = 80; sourceIp = "0.0.0.0/0"; }
            { fromPort = 443; toPort = 443; sourceIp = "0.0.0.0/0"; }
        ];
    };

    webserver = { resources, ... }: {
        inherit region;
        deployment.targetEnv = "ec2";
        deployment.ec2.region = region;
        deployment.ec2.instanceType = "t2.micro";
        deployment.ec2.ebsInitialRootDiskSize = 30;
        deployment.ec2.elasticIPv4 = resources.elasticIPs.webserver;
        deployment.ec2.keyPair = /* XXX: fill this in */;
        deployment.ec2.securityGroups = [ resources.ec2SecurityGroups.webserver ];
        deployment.ec2.elasticIPv4 = resources.elasticIPs.webserver;
        # XXX: The host should have *two* DNS names: haverer.jml.io, and
        # static.jml.io. It's not clear how to specify this with nixops
        # https://github.com/NixOS/nixops/issues/174
        deployment.route53 = {
            hostName = "haverer.jml.io"
            ttl = 600
        };
    };

}
