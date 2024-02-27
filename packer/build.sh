#!/bin/sh
set -e

packer init alpine.pkr.hcl

packer build \
	-force alpine.pkr.hcl


