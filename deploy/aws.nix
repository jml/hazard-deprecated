let
    region = "us-west-2";  # Arbitrarily chosen
    # XXX: Already exists, should generate instead, and use personal creds.
    keyPair = "mumak aws";
    privateKey = "/Users/jml/.ssh/mumakaws.pem";
    accessKeyId = "default";  # From ~/.ec2-keys
in
rec {

    resources.elasticIPs.webserverIP = {
        inherit region accessKeyId;
    };

    resources.ec2SecurityGroups.http-and-ssh = {
        inherit region accessKeyId;
        name = "http-and-ssh";
        description = "Allow web and SSH traffic";
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
        deployment.targetEnv = "ec2";
        deployment.ec2.region = region;
        deployment.ec2.accessKeyId = accessKeyId;
        deployment.ec2.instanceType = "t2.micro";
        deployment.ec2.ebsInitialRootDiskSize = 30;
        deployment.ec2.securityGroups = [ resources.ec2SecurityGroups.http-and-ssh ];
        deployment.ec2.elasticIPv4 = resources.elasticIPs.webserverIP;
        deployment.ec2.tags = {
            Name = "hazard";
        };
        deployment.ec2.keyPair = keyPair;
        deployment.ec2.privateKey = privateKey;
    };
}
