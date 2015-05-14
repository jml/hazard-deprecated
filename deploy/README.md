# Deployment

## Prerequisites

* ansible 1.5+
* fabric 1.10.1
* An AWS account

## Requirements

* One button build step
* One button deploy build step
* Deploy to laptop and run clients from it
* Version controlled configuration
* No secrets in version control
* Develop on OS X 10.10
* As cheap to run in production as possible
* One button rollbacks

## Procedure

### Provision a service

* Install the
[ec2 inventory script](http://docs.ansible.com/intro_dynamic_inventory.html#example-aws-ec2-external-inventory-script)
* Make sure `localhost` is listed in a file under `/etc/ansible/hosts`
* `ansible-playbook provisioning.yml`

**Note**: Assumes the key is called "mumak aws"

### Deploy to production

```
ansible-playbook \
  -e local_ssl_certificate_path=$PATH_TO_CERT \
  -e local_ssl_key_path=$PATH_TO_KEY \
  -e local_ssl_intermediates_path=$PATH_TO_INTERMEDIATE_CERTIFICATES \
  production.yml
```

This will set up all the hosts that are tagged with `Name=hazard`.

For EC2, need to provide path to private SSH key using `--private-key`.

## References

### General ideas

* [The Twelve-Factor App](http://12factor.net/)

### Ansible

* [AWS guide](http://docs.ansible.com/guide_aws.html)

## Alternatives considered

I tried using [Nix](http://nixos.org/nix/) in the hope of being able to use
[NixOps](http://nixos.org/nixops/). It's really nice, but deploying from an OS
X laptop didn't work.


## TODO

### Harden egress rules

Need access to:

* ocsp.usertrust.com (178.255.83.1/32)
* nixos
* amazon mirror of debian archive (54.185.154.163/32, 54.185.203.246/32)
* any third-party auth provider
