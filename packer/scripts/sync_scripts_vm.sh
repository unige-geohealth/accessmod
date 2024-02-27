#!/bin/bash

# Sync <accessmod>/packer/scripts with <vm>/home/accessmod/scripts

rsync -avz -e 'ssh -p 2222' ./scripts accessmod@localhost:/home/accessmod/
